#!/bin/bash

Rscript -e 'targets::tar_make()'
cp flow_cartogram.png /ephemeral/
