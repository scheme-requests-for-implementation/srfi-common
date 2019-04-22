#!/bin/bash

curl \
  --data "{\"action\":\"create\",\"entity\":\"list\",\"params\":[{\"name\":\"srfi-$SRFI\",\"archive_enabled\":true,\"archive_protected\":false,\"archive_spammode\":true,\"message_footer\":\"\",\"moderate\":6,\"restrict_post_lists\":[\"srfi-$SRFI\",\"srfi-auto-subscribe\"],\"subs_memberview\":\"\"}]}" \
  --header 'Content-Type: application/json' \
  --header "Authorization: Bearer $TOKEN" \
  --url https://www.simplelists.com/api/api.php
