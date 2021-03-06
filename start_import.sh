#!/bin/sh

#http://download.geofabrik.de/europe/germany-latest.osm.pbf
OSM_DOWNLOAD_FILE=berlin-latest.osm.pbf
OSM_DOWNLOAD_URL=http://download.geofabrik.de/europe/germany/${OSM_DOWNLOAD_FILE}

OSM2PGSQL_BIN=/usr/bin/osm2pgsql

OSM_LOCAL_FILE=openstreetmap-latest.osm.pbf
OSM_FILTERED_FILE=openstreetmap-filtered.osm.pbf
OSM_FILTER_EXPRESSIONS=filter-expressions.txt
OSM_LUA_SCRIPT=highways.lua
OSM_POSTPROCESS_SCRIPT=db_scripts.sql

## using database credentials from ~/.pg_service.conf
export PGSERVICE=osmdb


#
if [ ! -f "${OSM_LOCAL_FILE}" ]; then
  echo "downloading ${OSM_DOWNLOAD_URL}"
  wget -q ${OSM_DOWNLOAD_URL} -O ${OSM_LOCAL_FILE}
else
  echo "file ${OSM_LOCAL_FILE} exists"
fi

if [ -f "${OSM_LOCAL_FILE}" ]; then
  echo "processing osm data"
  osmium tags-filter -O -o ${OSM_FILTERED_FILE} -e ${OSM_FILTER_EXPRESSIONS} ${OSM_LOCAL_FILE}
else
  echo "file ${OSM_LOCAL_FILE} or ${OSM_FILTER_EXPRESSIONS} not found"
fi

if [ -f "${OSM_FILTERED_FILE}" ]; then
  echo "import osm data into db"
  ${OSM2PGSQL_BIN} -c -O flex -S ${OSM_LUA_SCRIPT} ${OSM_FILTERED_FILE}
fi

if [ -f "${OSM_POSTPROCESS_SCRIPT}" ]; then
  echo "postprocess osm db data"
  psql -q -f "${OSM_POSTPROCESS_SCRIPT}"
else
  echo "import script not found"
fi
