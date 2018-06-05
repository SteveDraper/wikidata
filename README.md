# wikidata

## Overview

```
wikidata - a processor for wikidata dump JSON

Usage: wikidata-exe [-f|--file INPUT_FILE] [-r|--records MAX_RECORDS]
                    [--claimFilter CLAIM_FILTER] [-o|--outdir OUTPUT_DIRECTORY]
                    [--claimsDest OUTPUT_FILE] [--mappingsDest OUTPUT_FILE]
                    [-i|--include INCLUDE_FLAGS] [--noCompress] [--sql]
  Process Wikidata dump JSON

Available options:
  -f,--file INPUT_FILE     File to read from (or stdin if omitted)
  -r,--records MAX_RECORDS Max JSON records to read (default all)
  --claimFilter CLAIM_FILTER
                           Space-separated list of retained relation ids (all if
                           absent)
  -o,--outdir OUTPUT_DIRECTORY
                           Where to place the ouput file(s). Defaults to current
                           directory
  --claimsDest OUTPUT_FILE Filename for the extracted claims triples (defaults
                           to 'claims.rdf')
  --mappingsDest OUTPUT_FILE
                           Filename for the extracted entity mappings (defaults
                           to 'entityMappings.rdf')
  -i,--include INCLUDE_FLAGS
                           What to include as a string of flag characters. 'c'
                           == claims, 'e' == entities. If absent all known
                           extractions will be performed
  --noCompress             Do not compress
  --sql                    Format for insertion into SQL tables (default off)
  -h,--help                Show this help text
```

Outputs the following, each to its own output file:

* WikiData id -> English Wikipedia Title
* Claims as triples of Wikidata id -> Relationship id -> Wikidata id

Only entities that are resolvable to English wikipedia are included.

Options allow:

* Filtering to a specified subset of claim types (aka 'Relationship id' above)
* Filtering of the set of output files produced to a subset
* Output as SQL statements for application to a SQL DB

## SQL
If the `--sql` option is specified the output files will be `.sql` files for each table suitable for insertion (recommended to turn off index updating around the insertion) into a SQL DB with pre-existing tables.  The assumed table names and schema are as follows:

### Entity mappings

Table name: `WikiMappings` (utf-encoding)

Fields:

	`entity` (VARCHAR(255))
	`wikiTitle` (VARCHAR(255))

Example creation statement:

```
CREATE TABLE WikiMappings (
  entity VARCHAR(255) NOT NULL,
  wikiTitle VARCHAR(255) NOT NULL,
  PRIMARY KEY (entity)
)

```

Table name: `Claims` (utf-encoding)

Fields:

	`entity` (VARCHAR(255))
	`relation` (VARCHAR(30))
	`target` (VARCHAR(255))

Example creation statement:

```
CREATE TABLE Claims (
  entity VARCHAR(255) NOT NULL,
  relation VARCHAR(30) NOT NULL,
  target VARCHAR(255) NOT NULL,
  PRIMARY KEY (relation, entity),
  KEY (entity)
)

```

## Notes
Since a full wikidata dump is over half a TB of JSON the process is necessarily streamed, and has O(1) memory footprint.  On my Mac it processes about 1000 raw entities per second (each of which consists of about 3000 lines of pretty-printed JSON if expanded directly with a pretty printer)