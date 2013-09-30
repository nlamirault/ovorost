#!/bin/sh

echo "Creation utilisateur Ovorost"
createuser -P ovorost

echo "Creation des differentes base de donnees"
createdb -O ovorost ovorost_local
createdb -O ovorost ovorost_dev
createdb -O ovorost ovorost_test
createdb -O ovorost ovorost
createdb -O ovorost ovorost_ut
