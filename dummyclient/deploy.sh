#!/bin/bash

rebar3 as prod release
tar -czf client.tgz _build/prod/rel/dummyclient setup.sh
sudo mv client.tgz /var/www/html
