#

## Exploring the `__mist__.json` with jq

To list all unique `stmt_type`,

```powershell
jq -C '.. | select(objects | has(""stmt_type"")) | .stmt_type' '__mist__.json' | Sort-Object | Get-Unique
```

To list all unique `expr_type`,

```powershell
jq -C '.. | select(objects | has(""expr_type"")) | .expr_type' '__mist__.json' | Sort-Object | Get-Unique
```

To list all unique `token_type`,

```powershell
jq -C '.. | select(objects | has(""token_type"")) | .token_type' '__mist__.json' | Sort-Object | Get-Unique
```
