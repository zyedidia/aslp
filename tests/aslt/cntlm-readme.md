to generate cntlm-isns.txt:
```bash
llvm-objdump -D ~/Downloads/cntlm/cntlm --section=.text | grep ': ' | cut -d' ' -f4,5- | sort | uniq
```
to generate cntlm-ops.txt:
```bash
cat cntlm-isns.txt | cut -d' ' -f1 | xargs printf '0x%s\n'
```