CREATE ALGORITHM=UNDEFINED DEFINER=`datendesign`@`localhost` SQL SECURITY DEFINER VIEW `v_za4972_laender` AS (select (case when (`za4972`.`v7` in (4,14)) then 4 when (`za4972`.`v7` in (9,10)) then 10 else `za4972`.`v7` end) AS `v7`,avg(`za4972`.`v84`) AS `v84`,avg(`za4972`.`v85`) AS `v85`,avg((case when (`za4972`.`v100` = 1) then 1 when (`za4972`.`v100` = 2) then -(1) when (`za4972`.`v100` = 3) then 0 end)) AS `v100`,avg((case when (`za4972`.`v114` = 1) then 1 when (`za4972`.`v114` = 2) then -(1) when (`za4972`.`v114` = 3) then 0 end)) AS `v114` from `za4972` group by (case when (`za4972`.`v7` in (4,14)) then 4 when (`za4972`.`v7` in (9,10)) then 10 else `za4972`.`v7` end));