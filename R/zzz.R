.onLoad <- function(libname, pkgname) {

  # Create cached version of package functions
  get_rosson_access_key_cached <<- memoise::memoise(get_rosson_access_key,
                                                    cache = cachem::cache_mem(max_age = 604800)) # 7d

  get_rosson_latest_version_cached <<- memoise::memoise(get_rosson_latest_version,
                                                        cache =cachem::cache_mem(max_age = 86400)) # 24h

  # Cache function with long execution time
  get_rosson_riders <<- memoise::memoise(get_rosson_riders,
                                         cache =cachem::cache_mem(max_age = 86400)) # 24h

  get_fnch_class_results <<- memoise::memoise(get_fnch_class_results)

  get_fnch_rider_infos <<- memoise::memoise(get_fnch_rider_infos,
                                            cache =cachem::cache_mem(max_age = 604800)) # 7d
}
