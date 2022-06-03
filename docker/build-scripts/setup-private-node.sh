#!/bin/bash

# Copy private node config
if [[ $PRIVATE_NODE == true ]]; then
  mkdir -p /etc/waves
  cp /tmp/waves.conf /etc/waves/waves.conf
fi

# Cleanup
rm -rf /tmp/*
