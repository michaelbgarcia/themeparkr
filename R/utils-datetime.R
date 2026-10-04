# Internal: parse ISO 8601 timestamps from the API ("2026-10-04T14:23:11.123Z"
# or with a "-04:00" offset) to POSIXct in UTC. Unparseable values become NA.
tpr_datetime = function(x) {
  x = sub("Z$", "+0000", x)
  x = sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")
}
