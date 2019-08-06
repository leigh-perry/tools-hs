# tools

## Development

ghcid --command "stack ghci --ghci-options=-fno-warn-unused-binds"

## Usage

### Parse SQL

```
stack run parse ~/dev/iag/serenity/integration-poc/sql/select-1.sql \
  '["reftable_1","reftable_2","reftable_3""]'
```

### Generate Avro schema

```
stack run gen-schema TABLENAME ~/temp/test.avsc
```
