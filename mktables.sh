#!/bin/bash

echo percision and recall:;
cut -f1,2 glade.txt > gladePandR;
cut -f1,2 learner.txt > learnerPandR;
paste gladePandR learnerPandR | sed -E "s/\\t/&/g"