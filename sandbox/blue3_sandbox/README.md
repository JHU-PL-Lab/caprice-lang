# Blue3 Sandbox

Some helper scripts for informal benchmarks / playing around with Blue3 api.

## Benchmarks
Benchmark Blue3 solver vs Z3 only solver. It outputs the SQLite insert SQL text into a "benchmarks" table to stderr.

```bash
dune exec ./benchmarks.exe -- 2000 < formulas.txt \
2> >(tail -n +3 > benchmarks.sql)
```

### One shot analysis markdown
You can create the analysis output results from `analysis.sql` formatted in markdown like this:
```bash
{
  cat formulas.txt | dune exec ./benchmarks.exe -- 2000 1>/dev/null 2> >(tail -n +3)
  cat analysis.sql
} | sqlite3 ":memory:" > analysis.out
```

## Sanity Check
To check the models the blue3 solver outputs are actually satisfiable:

```bash
cat formulas.txt | dune exec ./sanity_check.exe
```

If it prints:

```bash
checks out!
```

Then the solver is OK. Otherwise, it will print out the inconsistent formula ids.
