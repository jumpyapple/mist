# Mist

A command line oriented tool to work with [Fields of Mistria](https://www.fieldsofmistria.com/)'s `__mist__.json` file.

## Feature

- Convert Mist in JSON to a more human-readable format.
- Generate JSON Mist from said human-readable format.
- List the names of all the mists in the file.

## Note

### Exploring the `__mist__.json` with jq

To list all top-level keys,

```shell
jq 'keys' __mist__.json > keys.json
```

To list all unique `stmt_type`, `expr_type` and `token_type`,

```powershell
$filename = '__mist__.json'
jq -C '.. | select(objects | has(""stmt_type"")) | .stmt_type' $filename | Sort-Object | Get-Unique;
jq -C '.. | select(objects | has(""expr_type"")) | .expr_type' $filename | Sort-Object | Get-Unique
jq -C '.. | select(objects | has(""token_type"")) | .token_type' $filename | Sort-Object | Get-Unique
```

For fish shell,

```shell
set filename = '__mist__.json'
jq -C '.. | select(objects | has("stmt_type")) | .stmt_type' $filename | sort | uniq
jq -C '.. | select(objects | has("expr_type")) | .expr_type' $filename | sort | uniq
jq -C '.. | select(objects | has("token_type")) | .token_type' $filename | sort | uniq
```

To list all unique `expr_type`,

```powershell
```

To list all unique `token_type`,

```powershell

```
### Extracting all mists

Fish shell only

```shell
set mist_names $(jq -r 'keys | .[]' ./mists/__mist__.json)

for mist_name in $mist_names 
  echo "Extracting $mist_name";
  jq '. | with_entries(select(.key | startswith("'"$mist_name"'")))' ./mists/__mist__.json | uvx compact-json --output "./mists/$mist_name.json" -
end
```

Other related commands

```shell
set mist_name "adeline_eight_hearts.mist"

for mist_name in $mist_names 
  echo "Processing $mist_name";
  ./target/debug/mist.exe "./mists/$mist_name.json" > "./output/$mist_name.txt";
end

for mist_name in $mist_names
  echo "Checking $mist_name";
  head -n2 "./output/$mist_name.txt";
  echo "";
end
```
