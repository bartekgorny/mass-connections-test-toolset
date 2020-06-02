#!/bin/bash

cd _build/prod/rel/dummyclient/

host=$(hostname -i)

sed -i -e s/dummyclient$/dummyclient@$host/ releases/0.1.0/vm.args
export HOME=/root
ulimit -n 100000
sysctl -w net.ipv4.ip_local_port_range="5000 62000"
bin/dummyclient daemon &
