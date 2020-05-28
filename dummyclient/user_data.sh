#!/bin/bash

# this code is set as user_data in AWS launch tempplate
# so that it is run when an instance is created

wget 10.2.100.199/client.tgz
tar -xzf client.tgz
./setup.sh


