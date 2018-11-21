# muddy
muddy is a server for live voting on muddiest point during lectures


# how to test

sudo tcpdump -i eno1 host bjornux.cs.lth.se

sudo tcpdump -i eno1 host 130.235.136.43 | grep -v mdns
