INSERT INTO types (name) VALUES
('video'),
('book'),
('podcast')
;

INSERT INTO nodes (id, name) VALUES
('0000000000000000000000000000000000000000000000000000','node0'),
('1111111111111111111111111111111111111111111111111111',NULL)
;

INSERT INTO entries (cid, name, url, type) VALUES
('bafybeif4qnbp4a2shsmkmlsmy5fqx5tizrn75nqgajvpb7h56fxbgsgvbu','Program Design by Calculation','https://www4.di.uminho.pt/~jno/ps/pdbc.pdf','book'),
('bafybeid3iyv2jrlttygj2xhdjcqi2oz4lpc6mtlwjemc7muyvcewf5rn2i','Geographis of Digital Exclusion','https://www.oii.ox.ac.uk/research/publications/geographies-of-digital-exclusion-data-and-inequality','book'),
('bafybeidi44wekwu3bi24m5x6ofnttzbcsfomr4dw5bssovqq35wdwva26m','Nothing to Hide','https://vidcommons.org/w/efeEpsHSK3bzJwVLW9fh7U','video'),
('bafybeibkjcszoeok55itfyxfwrasjxylctzlb5lcidinoo35igbd67xyse','Flash Forward: Robocop','https://www.flashforwardpod.com/2017/05/02/robocop','podcast'),
('bafybeigx4kion4prcfn47k64zorgimenua7vnwpseiy5266nhp2ppzn7li','TPB: AFK',NULL,'video')
;

INSERT INTO pins (node, cid) VALUES
('0000000000000000000000000000000000000000000000000000','bafybeif4qnbp4a2shsmkmlsmy5fqx5tizrn75nqgajvpb7h56fxbgsgvbu'),
('1111111111111111111111111111111111111111111111111111','bafybeif4qnbp4a2shsmkmlsmy5fqx5tizrn75nqgajvpb7h56fxbgsgvbu'),
('0000000000000000000000000000000000000000000000000000','bafybeid3iyv2jrlttygj2xhdjcqi2oz4lpc6mtlwjemc7muyvcewf5rn2i'),
('1111111111111111111111111111111111111111111111111111','bafybeidi44wekwu3bi24m5x6ofnttzbcsfomr4dw5bssovqq35wdwva26m'),
('0000000000000000000000000000000000000000000000000000','bafybeigx4kion4prcfn47k64zorgimenua7vnwpseiy5266nhp2ppzn7li')
;
