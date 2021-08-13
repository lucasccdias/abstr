# Librarys ----------------------------------------------------------------
library(tidyverse)
library(sf)
library(stplanr)
library(od)

# Dont use s2 engine
sf::sf_use_s2(FALSE)

# Site info ---------------------------------------------------------------
site_name = "lcid"
site = sf::read_sf("https://raw.githubusercontent.com/cyipt/actdev/main/data-small/lcid/site.geojson") # Read site geojson from actdev repo

##### GENERAL VARIABLES
max_length = 20000 # maximum length of desire lines in m
household_size = 2.3 # mean UK household size at 2011 census
min_flow_routes = 2 # threshold above which OD pairs are included
region_buffer_dist = 2000
large_area_buffer = 500

# Useful functions
smart.round = function(x) {
  y = floor(x)
  indices = utils::tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] = y[indices] + 1
  y
}

# Download town centre data -----------------------------------------------
download.file("http://maps.communities.gov.uk/geoserver/dclg_inspire/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=dclg_inspire%3AEnglish_Town_Centres_2004&outputFormat=SHAPE-ZIP","dataset.zip")
unzip("dataset.zip", exdir = "data")
# Read and transform town centre data
town_centres = st_read("data/English_Town_Centres_2004.shp")
st_transform(town_centres, 4326)
st_precision(town_centres) = 1000000
town_centroids = town_centres %>%
  st_drop_geometry() %>%
  sf::st_as_sf(coords = c("CENTROIDX", "CENTROIDY"), crs = 27700) %>%
  st_transform(4326)


# Download msoa centroids data --------------------------------------------
centroids_msoa = pct::get_centroids_ew()
centroids_msoa = sf::st_transform(centroids_msoa, 4326)
zones_msoa_national = pct::get_pct(national = TRUE, geography = "msoa", layer = "z")
sf::st_crs(zones_msoa_national)
st_precision(zones_msoa_national) = 1000000
site_centroid = site %>%
  st_transform(27700) %>%
  st_centroid() %>%
  st_transform(4326)
# Download od data from uk census -----------------------------------------
od = pct::get_od()

# Download msoa population data --------------------------------------------
msoa_pops = readxl::read_xls(path = "../../../Downloads/mid2011msoaunformattedfile.xls", sheet = "Mid-2011 Persons", )
msoa_pops = msoa_pops %>%
  select(geo_code1 = Code, msoa_population = "All Ages")
site_pops = site %>%
  st_drop_geometry() %>%
  mutate(site_population = dwellings_when_complete * household_size)


# Zones near site ---------------------------------------------------------
zones_touching_site = zones_msoa_national[site, , op = sf::st_intersects]
zones_touching_site$overlap_size = units::drop_units(st_area(zones_touching_site))
zones_touching_site = zones_touching_site %>%
  filter(overlap_size > 10000) %>%
  select(-overlap_size)

zone_data = zones_touching_site %>%
  st_drop_geometry() %>%
  mutate(site_name = site$site_name)

site_c = right_join(site_centroid, zone_data) %>%
  select(geo_code, site_name)

# Generate desire lines ---------------------------------------------------
od_site = od %>%
  filter(geo_code1 %in% zones_touching_site$geo_code) %>%
  filter(geo_code2 %in% centroids_msoa$msoa11cd)
desire_lines_site = od::od_to_sf(x = od_site, z = site_c, zd = centroids_msoa) %>%
  mutate(site_name = site_name)

# Adjust flows to represent site population, not MSOA population(s)
# for both MSOAs and development sites, these are entire populations, not commuter populations
desire_lines_site = inner_join(desire_lines_site, msoa_pops)
desire_lines_site = inner_join(desire_lines_site, site_pops)

site_population = unique(desire_lines_site$site_population)
unique_msoa_pops = desire_lines_site %>%
  st_drop_geometry() %>%
  select(geo_code1, msoa_population) %>%
  unique()
sum_msoa_pops = sum(unique_msoa_pops$msoa_population)
desire_lines_site = desire_lines_site %>%
  mutate(sum_msoa_pops = sum_msoa_pops)

# keeping converted flows in the original columns
desire_lines_pops = desire_lines_site %>%
  mutate(across(all:other, .fns = ~ ./ sum_msoa_pops * site_population))

desire_lines_combined = desire_lines_pops %>%
  sf::st_drop_geometry() %>%
  mutate(geo_code1 = site_name) %>%
  group_by(geo_code1, geo_code2) %>%
  summarise(
    across(all:other, sum)
  )
desire_lines_combined = od::od_to_sf(x = desire_lines_combined, z = site, zd = centroids_msoa)

desire_lines_combined$length = round(stplanr::geo_length(desire_lines_combined))

desire_lines_combined = desire_lines_combined %>%
  mutate(
    trimode_base = foot + bicycle + car_driver,
    pwalk_base = foot/trimode_base,
    pcycle_base = bicycle/trimode_base,
    pdrive_base = car_driver/trimode_base
  )
desire_lines_combined[is.na(desire_lines_combined)] = 0

desire_lines_combined = desire_lines_combined %>%
  select(geo_code1, geo_code2, all, trimode_base, from_home:other, length, pwalk_base:pdrive_base) %>%
  mutate(across(where(is.numeric), round, 6))

st_precision(desire_lines_combined) = 1000000

# Round decimals and select sets of desire lines --------------------------
desire_lines_rounded = desire_lines_combined %>%
  rename(all_base = all, walk_base = foot, cycle_base = bicycle, drive_base = car_driver) %>%
  mutate(
    across(all_base:other, smart.round),
    trimode_base = walk_base + cycle_base + drive_base
  ) %>%
  filter(trimode_base > 0)

desire_lines_20km = desire_lines_rounded %>%
  filter(length <= max_length)

desire_lines_threshold = desire_lines_rounded %>%
  filter(trimode_base >= min_flow_routes)

desire_lines_bounding = desire_lines_20km %>%
  filter(geo_code2 %in% desire_lines_threshold$geo_code2)

# Large study area MSOAs --------------------------------------------------
large_study_area = sf::st_convex_hull(sf::st_union(desire_lines_bounding))
large_study_area = large_study_area %>%
  sf::st_transform(27700) %>%  # use local projected CRS
  sf::st_buffer(dist = large_area_buffer) %>%
  sf::st_transform(4326)

desire_lines_many = desire_lines_rounded[large_study_area, , op = sf::st_within]

desire_lines_many = desire_lines_many %>%
  select(geo_code1, geo_code2, all_base, trimode_base, walk_base, cycle_base, drive_base, length, pwalk_base:pdrive_base)

# Create routes and generate Go Dutch scenario ---------------------
obj = desire_lines_many %>% select(-length)
obj2 = desire_lines_many %>% filter(length < 6000) %>% select(-length)


routes_fast = stplanr::route(l = obj, route_fun = cyclestreets::journey)
routes_balanced = stplanr::route(l = obj, route_fun = cyclestreets::journey, plan = "balanced")
routes_quiet = stplanr::route(l = obj, route_fun = cyclestreets::journey, plan = "quietest")
routes_walk = stplanr::route(l = obj2, route_fun = stplanr::route_osrm)


routes_fast = routes_fast %>%
  mutate(busyness = busynance / distances) %>%
  group_by(geo_code1, geo_code2) %>%
  mutate(
    n = n(), #could remove
    mean_gradient = weighted.mean(gradient_smooth, distances),
    max_gradient = max(gradient_smooth),
    mean_busyness = weighted.mean(busyness, distances),
    max_busyness = max(busyness)
  ) %>%
  ungroup() %>%
  mutate(
    pcycle_godutch_uptake = pct::uptake_pct_godutch_2020(distance = length, gradient = mean_gradient),
    cycle_godutch_additional = pcycle_godutch_uptake * drive_base,
    cycle_godutch = cycle_base + cycle_godutch_additional,
    pcycle_godutch = cycle_godutch / all_base,
    drive_godutch = drive_base - cycle_godutch_additional, # ensure totals add up
    across(c(mean_gradient, max_gradient, mean_busyness, max_busyness, busyness, gradient_smooth, pcycle_godutch), round, 6)
  )

# to round cycle_godutch
routes_fast_summarised = routes_fast %>%
  st_drop_geometry() %>%
  group_by(geo_code2) %>%
  summarise(
    all_base = mean(all_base),
    trimode_base = mean(trimode_base),
    cycle_base = mean(cycle_base),
    cycle_godutch = mean(cycle_godutch)
  )
routes_fast_summarised = routes_fast_summarised %>%
  mutate(cycle_godutch = smart.round(cycle_godutch))


routes_fast = inner_join((routes_fast %>% select(-all_base, -trimode_base, -cycle_base, -cycle_godutch)), routes_fast_summarised)

# balanced routes
routes_balanced = routes_balanced %>%
  mutate(busyness = busynance / distances) %>%
  group_by(geo_code1, geo_code2) %>%
  mutate(
    n = n(), #could remove
    mean_gradient = weighted.mean(gradient_smooth, distances),
    max_gradient = max(gradient_smooth),
    mean_busyness = weighted.mean(busyness, distances),
    max_busyness = max(busyness)
  ) %>%
  ungroup() %>%
  mutate(
    pcycle_godutch_uptake = pct::uptake_pct_godutch_2020(distance = length, gradient = mean_gradient),
    cycle_godutch = pcycle_godutch_uptake * drive_base + cycle_base,
    pcycle_godutch = cycle_godutch / all_base,
    across(c(mean_gradient, max_gradient, mean_busyness, max_busyness, busyness, gradient_smooth, pcycle_godutch), round, 6)
  )

# to round cycle_godutch
routes_balanced_summarised = routes_balanced %>%
  st_drop_geometry() %>%
  group_by(geo_code2) %>%
  summarise(
    all_base = mean(all_base),
    trimode_base = mean(trimode_base),
    cycle_base = mean(cycle_base),
    cycle_godutch = mean(cycle_godutch)
  )
routes_balanced_summarised = routes_balanced_summarised %>%
  mutate(cycle_godutch = smart.round(cycle_godutch))

routes_balanced_summarised = routes_balanced_summarised %>%
  filter(cycle_base > 0 | cycle_godutch > 0) # remove routes with no cyclists under Go Dutch

routes_balanced = inner_join((routes_balanced %>% select(-all_base, -trimode_base, -cycle_base, -cycle_godutch)), routes_balanced_summarised)

# quiet routes
routes_quiet = routes_quiet %>%
  mutate(busyness = busynance / distances) %>%
  group_by(geo_code1, geo_code2) %>%
  mutate(
    n = n(), #could remove
    mean_gradient = weighted.mean(gradient_smooth, distances),
    max_gradient = max(gradient_smooth),
    mean_busyness = weighted.mean(busyness, distances),
    max_busyness = max(busyness)
  ) %>%
  ungroup() %>%
  mutate(
    pcycle_godutch = pct::uptake_pct_godutch_2020(distance = length, gradient = mean_gradient),
    cycle_godutch = pcycle_godutch * trimode_base,
    across(c(mean_gradient, max_gradient, mean_busyness, max_busyness, busyness, gradient_smooth, pcycle_godutch), round, 6)
  )

# to round cycle_godutch
routes_quiet_summarised = routes_quiet %>%
  st_drop_geometry() %>%
  group_by(geo_code2) %>%
  summarise(
    all_base = mean(all_base),
    trimode_base = mean(trimode_base),
    cycle_base = mean(cycle_base),
    cycle_godutch = mean(cycle_godutch)
  )
routes_quiet_summarised = routes_quiet_summarised %>%
  mutate(cycle_godutch = smart.round(cycle_godutch))

routes_quiet_summarised = routes_quiet_summarised %>%
  filter(cycle_base > 0 | cycle_godutch > 0) # remove routes with no cyclists

routes_quiet = inner_join((routes_quiet %>% select(-all_base, -trimode_base, -cycle_base, -cycle_godutch)), routes_quiet_summarised)

# # Walking route fixes if another routing service is used
if(is.null(routes_walk$distance)) {
  # change names if routing service used different names
  # but for google these columns are full of NAs
  routes_walk = routes_walk_save %>%
    mutate(distance = distance_m, duration = duration_s)
}

routes_walk_save = routes_walk %>%
  dplyr::filter(distance <= 6000 & trimode_base > 0) %>%
  mutate(
    # pwalk_base = walk_base / trimode_base,
    pwalk_godutch = case_when(
      distance <= 2000 ~ pwalk_base + 0.3, # 30% shift walking for routes >2km
      distance <= 2500 ~ pwalk_base + 0.2, # 20% shift walking for routes >2.5km
      distance <= 3000 ~ pwalk_base + 0.1, # 10% shift walking for routes >3km
      distance <= 6000 ~ pwalk_base + 0.05, # 5% shift walking for routes 3-6km
      TRUE ~ pwalk_base),
    walk_godutch = pwalk_godutch * trimode_base
  )

routes_walk_save = routes_walk_save %>%
  mutate(walk_godutch = smart.round(walk_godutch)) %>%
  mutate(pwalk_godutch = round(pwalk_godutch, 6))

all_commuters_baseline = sum(routes_fast_summarised$all_base)
trimode_commuters_baseline = sum(routes_fast_summarised$trimode_base)
cycle_commuters_baseline = sum(routes_fast_summarised$cycle_base)
cycle_commuters_godutch = sum(routes_fast_summarised$cycle_godutch)
walk_commuters_baseline = sum(routes_walk_save$walk_base)
walk_commuters_godutch = sum(routes_walk_save$walk_godutch)
# drive_commuters_baseline = sum(routes_fast_summarised$drive_base)
# drive_commuters_godutch = sum(routes_fast_summarised$drive_godutch)

# Route to town centre ----------------------------------------------------
bng_site = site_centroid %>% st_transform(27700)
bng_town = town_centroids %>% st_transform(27700)
# record = case_when(
#   site_name == "ebbsfleet" ~ as.integer(860),
#   TRUE ~ st_nearest_feature(bng_site, bng_town)
# )
record = st_nearest_feature(bng_site, bng_town)
town_nearest = town_centroids[record, ]
# mapview(town_nearest)
town_nearest = town_nearest %>%
  select(town_name = NAME)

# number of trips to the town centre estimated to equal the total number of commuter trips
# todo: change the godutch so this is calculated instead of assumed
od_town = data.frame(
  site_name = site_centroid$site_name,
  town_name = town_nearest$town_name,
  all_base = all_commuters_baseline,
  trimode_base = trimode_commuters_baseline,
  walk_base = walk_commuters_baseline,
  walk_godutch = walk_commuters_godutch,
  cycle_base = cycle_commuters_baseline,
  cycle_godutch = cycle_commuters_godutch
)

desire_line_town = od::od_to_sf(x = od_town, z = site_centroid, zd = town_nearest)
# mapview(desire_line_town)

route_fast_town = stplanr::route(l = desire_line_town, route_fun = cyclestreets::journey)
route_balanced_town = stplanr::route(l = desire_line_town, route_fun = cyclestreets::journey, plan = "balanced")
route_quiet_town = stplanr::route(l = desire_line_town, route_fun = cyclestreets::journey, plan = "quietest")
# working again for a single route. We want this for the desire line, even if it's over 6km
if(walk_commuters_baseline > 0 | walk_commuters_godutch > 0) {
  route_walk_town = stplanr::route(l = desire_line_town, route_fun = stplanr::route_osrm)
}

fast_town = route_fast_town %>%
  mutate(
    busyness = busynance / distances,
    mean_gradient = weighted.mean(gradient_smooth, w = distances),
    max_gradient = max(gradient_smooth),
    mean_busyness = weighted.mean(busyness, w = distances),
    max_busyness = max(busyness),
    pcycle_godutch = cycle_godutch / trimode_base,
    across(where(is.numeric), round, 6)
  ) %>%
  select(site_name, town_name, all_base, trimode_base, cycle_base, cycle_godutch, distances,
         # time, speed,
         length, gradient_smooth, mean_gradient, max_gradient, busyness, mean_busyness, max_busyness, pcycle_godutch)

balanced_town = route_balanced_town %>%
  mutate(
    busyness = busynance / distances,
    mean_gradient = weighted.mean(gradient_smooth, w = distances),
    max_gradient = max(gradient_smooth),
    mean_busyness = weighted.mean(busyness, w = distances),
    max_busyness = max(busyness),
    pcycle_godutch = cycle_godutch / trimode_base,
    across(where(is.numeric), round, 6)
  ) %>%
  select(site_name, town_name, all_base, trimode_base, cycle_base, cycle_godutch, distances,
         # time, speed,
         length, gradient_smooth, mean_gradient, max_gradient, busyness, mean_busyness, max_busyness, pcycle_godutch)

quiet_town = route_quiet_town %>%
  mutate(
    busyness = busynance / distances,
    mean_gradient = weighted.mean(gradient_smooth, w = distances),
    max_gradient = max(gradient_smooth),
    mean_busyness = weighted.mean(busyness, w = distances),
    max_busyness = max(busyness),
    pcycle_godutch = cycle_godutch / trimode_base,
    across(where(is.numeric), round, 6)
  ) %>%
  select(site_name, town_name, all_base, trimode_base, cycle_base, cycle_godutch, distances,
         # time, speed,
         length, gradient_smooth, mean_gradient, max_gradient, busyness, mean_busyness, max_busyness, pcycle_godutch)

# Combine and save routes -------------------------------------------------
routes_fast_cutdown = routes_fast %>%
  select(geo_code1, geo_code2, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, busyness, gradient_smooth, pcycle_godutch)

fast_town_cutdown = fast_town %>%
  select(geo_code1 = site_name, geo_code2 = town_name, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, busyness, gradient_smooth, pcycle_godutch)

routes_fast_combined = bind_rows(
  routes_fast_cutdown %>% mutate(purpose = "commute"),
  fast_town_cutdown %>% mutate(purpose = "town")
)

routes_fast_entire = routes_fast_combined %>%
  group_by(geo_code1, geo_code2, purpose, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, pcycle_godutch) %>%
  summarise() %>%
  arrange(cycle_base)

desire_lines_lookup = desire_lines_many %>%
  sf::st_drop_geometry() %>%
  select(matches("geo|agg"))

# balanced routes
routes_balanced_cutdown = routes_balanced %>%
  select(geo_code1, geo_code2, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, busyness, gradient_smooth, pcycle_godutch)

balanced_town_cutdown = balanced_town %>%
  select(geo_code1 = site_name, geo_code2 = town_name, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, busyness, gradient_smooth, pcycle_godutch)

routes_balanced_combined = bind_rows(
  routes_balanced_cutdown %>% mutate(purpose = "commute"),
  balanced_town_cutdown %>% mutate(purpose = "town")
)

routes_balanced_entire = routes_balanced_combined %>%
  group_by(geo_code1, geo_code2, purpose, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, pcycle_godutch) %>%
  summarise() %>%
  arrange(cycle_base)

# quiet routes
routes_quiet_cutdown = routes_quiet %>%
  select(geo_code1, geo_code2, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, busyness, gradient_smooth, pcycle_godutch)

quiet_town_cutdown = quiet_town %>%
  select(geo_code1 = site_name, geo_code2 = town_name, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, busyness, gradient_smooth, pcycle_godutch)

routes_quiet_combined = bind_rows(
  routes_quiet_cutdown %>% mutate(purpose = "commute"),
  quiet_town_cutdown %>% mutate(purpose = "town")
)

routes_quiet_entire = routes_quiet_combined %>%
  group_by(geo_code1, geo_code2, purpose, length, mean_gradient, max_gradient, mean_busyness, max_busyness, all_base, trimode_base, cycle_base, cycle_godutch, pcycle_godutch) %>%
  summarise() %>%
  arrange(cycle_base)

# walking routes
if(walk_commuters_baseline > 0 | walk_commuters_godutch > 0) {
  # combine commute and town routes
  routes_walk_cutdown = routes_walk_save %>%
    select(geo_code1, geo_code2, distance, duration, all_base, trimode_base, walk_base, walk_godutch, pwalk_godutch)

  walk_town_cutdown = route_walk_town %>%
    mutate(pwalk_godutch = round(walk_godutch / trimode_base, 6)) %>%
    select(geo_code1 = site_name, geo_code2 = town_name, distance, duration, all_base, trimode_base, walk_base, walk_godutch, pwalk_godutch)

  routes_walk_combined = routes_walk_cutdown %>% mutate(purpose = "commute")

  if(route_walk_town$distance <= 6000) routes_walk_combined = bind_rows(routes_walk_combined, walk_town_cutdown %>% mutate(purpose = "town"))

  # create object for rnet and to save (desire lines simply use routes_walk_save)
  obj = routes_walk_combined %>%
    select(geo_code1, geo_code2, purpose, distance, duration, all_base, trimode_base, walk_base, walk_godutch)
  if(any(grepl(pattern = "^i|synthetic", x = obj$geo_code2))) {
    obj = obj %>%
      left_join(desire_lines_lookup) %>%
      mutate(geo_code1 = o_agg, geo_code2 = d_agg) %>%
      select(-matches("agg"))
  }
}

# Route networks ----------------------------------------------------------
rnet_fast = overline(routes_fast_combined, attrib = c("cycle_base", "cycle_godutch", "busyness", "gradient_smooth"), fun = c(sum, mean))
rnet_fast = rnet_fast %>%
  select(cycle_base = cycle_base_fn1, cycle_godutch = cycle_godutch_fn1, busyness = busyness_fn2, gradient_smooth = gradient_smooth_fn2) %>%
  mutate(gradient_smooth = round(gradient_smooth, 6)) %>%
  rename(gradient = gradient_smooth)
# nrow(rnet_fast)
# mapview::mapview(rnet_fast["cycle_base"])

rnet_balanced = overline(routes_balanced_combined, attrib = c("cycle_base", "cycle_godutch", "busyness", "gradient_smooth"), fun = c(sum, mean))
rnet_balanced = rnet_balanced %>%
  select(cycle_base = cycle_base_fn1, cycle_godutch = cycle_godutch_fn1, busyness = busyness_fn2, gradient_smooth = gradient_smooth_fn2) %>%
  mutate(gradient_smooth = round(gradient_smooth, 6)) %>%
  rename(gradient = gradient_smooth)

rnet_quiet = overline(routes_quiet_combined, attrib = c("cycle_base", "cycle_godutch", "busyness", "gradient_smooth"), fun = c(sum, mean))
rnet_quiet = rnet_quiet %>%
  select(cycle_base = cycle_base_fn1, cycle_godutch = cycle_godutch_fn1, busyness = busyness_fn2, gradient_smooth = gradient_smooth_fn2) %>%
  mutate(gradient_smooth = round(gradient_smooth, 6)) %>%
  rename(gradient = gradient_smooth)

if(walk_commuters_baseline > 0 | walk_commuters_godutch > 0) {
  # r_walk_grouped_lines = routes_walk_combined %>% st_cast("LINESTRING") #is this needed?
  rnet_walk = overline(routes_walk_combined, attrib = c("walk_base", "walk_godutch", "duration"), fun = c(sum, mean))
  rnet_walk = rnet_walk %>%
    select(walk_base = walk_base_fn1, walk_godutch = walk_godutch_fn1, duration = duration_fn2)
}

# Go Dutch scenario for desire lines -------------------------------------

# get the go dutch flows for cycle and walk commutes
join_fast = routes_fast_entire %>%
  select(geo_code2, purpose, cycle_godutch, pcycle_godutch) %>%
  st_drop_geometry()
desire_lines_scenario = inner_join(desire_lines_many, join_fast, by = "geo_code2")

join_walk = routes_walk_combined %>%
  st_drop_geometry() %>%
  select(geo_code2, walk_godutch, pwalk_godutch)
desire_lines_scenario = left_join(desire_lines_scenario, join_walk, by = "geo_code2")

desire_lines_scenario = desire_lines_scenario %>%
  select(geo_code1, geo_code2, purpose, all_base:length, walk_godutch, pwalk_godutch, cycle_godutch, pcycle_godutch)

# same number to town as commute - simplifying assumption
drive_commuters_baseline = sum(desire_lines_scenario$drive_base)

# add in a desire line to the town centre
desire_line_town = desire_line_town %>%
  mutate(
    purpose = "town",
    drive_base = drive_commuters_baseline,
    length = round(stplanr::geo_length(desire_line_town)),
    pwalk_godutch = walk_godutch / trimode_base,
    pcycle_godutch = cycle_godutch / trimode_base

  ) %>%
  rename(geo_code1 = site_name, geo_code2 = town_name) %>%
  select(geo_code1, geo_code2, purpose, all_base, trimode_base, walk_base, cycle_base, drive_base, length, walk_godutch, pwalk_godutch, cycle_godutch, pcycle_godutch)

desire_lines_final = bind_rows(
  desire_lines_scenario,
  desire_line_town
) %>%
  select(-matches("pw|pc|pd")) %>%
  mutate_if(is.numeric, round)

if(any(grepl(pattern = "agg", x = names(desire_lines_final)))) {
  desire_lines_final = desire_lines_final %>%
    mutate(geo_code1 = o_agg, geo_code2 = d_agg) %>%
    select(-matches("agg"))
}

desire_lines_final$purpose[is.na(desire_lines_final$purpose)] = "commute"
desire_lines_final[is.na(desire_lines_final)] = 0

excess_active = desire_lines_final$walk_godutch + desire_lines_final$cycle_godutch - desire_lines_final$trimode_base
sel_excess = excess_active > 0
desire_lines_final$cycle_godutch[sel_excess] = desire_lines_final$cycle_godutch[sel_excess] - pmin(excess_active[sel_excess], desire_lines_final$cycle_godutch[sel_excess])

excess_active = desire_lines_final$walk_godutch + desire_lines_final$cycle_godutch - desire_lines_final$trimode_base
sel_excess = excess_active > 0
desire_lines_final$walk_godutch[sel_excess] = desire_lines_final$walk_godutch[sel_excess] - pmin(excess_active[sel_excess], desire_lines_final$walk_godutch[sel_excess])

# calculate drive_godutch
desire_lines_final = desire_lines_final %>%
  mutate(
    trimode_base = walk_base + cycle_base + drive_base,
    drive_godutch = pmax(trimode_base - (cycle_godutch + walk_godutch), 0)
  )

min_flow_map = site_population / 80
desire_lines_busy = desire_lines_final %>%
  filter(trimode_base >= min_flow_map)

convex_hull = sf::st_convex_hull(sf::st_union(desire_lines_busy))
study_area = stplanr::geo_buffer(convex_hull, dist = region_buffer_dist)
st_precision(study_area) = 1000000

desire_lines_few = desire_lines_final[study_area, , op = sf::st_within]

leeds_od = desire_lines_few %>%
  sf::st_drop_geometry() %>%
  select(geo_code1, geo_code2, All = trimode_base, Walk = walk_base, Bike = cycle_base, Drive = drive_godutch) %>%
  mutate(Transit = All - Walk - Bike - Drive) %>%
  slice(1:3)
