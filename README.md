Trying something out.

### Snooping for git urls / tags

```
cabal info time \
    | awk -F': ' '/Version/ {print $0 > "/dev/stderr"} /git/ && !n {print $2; n=1}' \
    | xargs -tn1 git ls-remote
```

Works almost every time.
