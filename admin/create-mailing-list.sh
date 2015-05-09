#!/bin/bash

curl \
  --data "{\"action\":\"create\",\"entity\":\"list\",\"params\":[{\"name\":\"srfi-$NUMBER\",\"archive_enabled\":true,\"archive_spammode\":true,\"moderate\":6,\"restrict_post_lists\":[\"srfi-$NUMBER\",\"srfi-auto-subscribe\"],\"subs_memberview\":\"\"}]}" \
  --header 'Content-Type: application/json' \
  --header "Authorization: Bearer $TOKEN" \
  --url https://www.simplelists.com/api/api.php
