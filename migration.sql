-- ----------------------------------------------------------------------------
-- View fiberdb.members_estates
-- ----------------------------------------------------------------------------
USE `fiberdb`;
CREATE  OR REPLACE VIEW `fiberdb`.`members_estates` AS select `mp`.`memberid` AS `memberid`,`mp`.`name` AS `name`,`mp`.`contact` AS `contact`,`mp`.`note` AS `m_note`,`mp`.`fromyear` AS `m_fromyear`,`mp`.`toyear` AS `m_toyear`,`me`.`fromyear` AS `me_fromyear`,`me`.`toyear` AS `me_toyear`,`e`.`estateid` AS `estateid`,`e`.`location` AS `location`,`e`.`address` AS `address`,`e`.`note` AS `e_note`,`e`.`fromyear` AS `e_fromyear`,`e`.`toyear` AS `e_toyear` from (`fiberdb`.`membersestates` `me` join (`fiberdb`.`member_pref` `mp` join `fiberdb`.`estates` `e`) on(((`mp`.`memberid` = `me`.`memberid`) and (`e`.`estateid` = `me`.`estateid`))));

-- ----------------------------------------------------------------------------
-- View fiberdb.current_full
-- ----------------------------------------------------------------------------
USE `fiberdb`;
CREATE  OR REPLACE VIEW `fiberdb`.`current_full` AS select distinct `me`.`memberid` AS `memberid`,`me`.`name` AS `name`,`me`.`contact` AS `contact`,`me`.`m_note` AS `m_note`,`me`.`m_fromyear` AS `m_fromyear`,`me`.`m_toyear` AS `m_toyear`,`me`.`me_fromyear` AS `me_fromyear`,`me`.`me_toyear` AS `me_toyear`,`me`.`estateid` AS `estateid`,`me`.`location` AS `location`,`me`.`address` AS `address`,`me`.`e_note` AS `e_note`,`me`.`e_fromyear` AS `e_fromyear`,`me`.`e_toyear` AS `e_toyear`,`eb`.`bimonths` AS `bimonths` from (`fiberdb`.`members_estates` `me` join `fiberdb`.`estatebi` `eb` on((`eb`.`estateid` = `me`.`estateid`))) where (isnull(`me`.`me_toyear`) and (`eb`.`year` = year(curdate())));

-- ----------------------------------------------------------------------------
-- View fiberdb.estate_owners
-- ----------------------------------------------------------------------------
USE `fiberdb`;
CREATE  OR REPLACE VIEW `fiberdb`.`estate_owners` AS select `mp`.`memberid` AS `memberid`,`mp`.`name` AS `name`,`mp`.`note` AS `note`,`mp`.`contact` AS `contact`,`me`.`estateid` AS `estateid`,`me`.`fromyear` AS `fromyear`,`me`.`toyear` AS `toyear` from (`fiberdb`.`member_pref` `mp` join `fiberdb`.`membersestates` `me` on((`me`.`memberid` = `mp`.`memberid`)));

-- ----------------------------------------------------------------------------
-- View fiberdb.estate_sums
-- ----------------------------------------------------------------------------
USE `fiberdb`;
CREATE  OR REPLACE VIEW `fiberdb`.`estate_sums` AS select `es`.`conamount` AS `conamount`,`es`.`contax` AS `contax`,`es`.`conmonths` AS `conmonths`,`es`.`opamount` AS `opamount`,`es`.`optax` AS `optax`,`es`.`opmonths` AS `opmonths`,`es`.`payamount` AS `payamount`,`es`.`estateid` AS `estateid`,`es`.`year` AS `year`,`es`.`total` AS `total`,`me`.`address` AS `address`,`me`.`memberid` AS `memberid` from (`fiberdb`.`estatedc_sums` `es` join `fiberdb`.`member_estates` `me` on((`me`.`estateid` = `es`.`estateid`)));

-- ----------------------------------------------------------------------------
-- View fiberdb.estate_usage
-- ----------------------------------------------------------------------------
USE `fiberdb`;
CREATE  OR REPLACE VIEW `fiberdb`.`estate_usage` AS select distinct `eo`.`memberid` AS `memberid`,`eo`.`name` AS `name`,`eo`.`contact` AS `contact`,`eo`.`fromyear` AS `fromyear`,`eo`.`toyear` AS `toyear`,`e`.`estateid` AS `estateid`,`e`.`address` AS `address`,`eb`.`bimonths` AS `bimonths`,`ea`.`actmonths` AS `actmonths`,`eb`.`year` AS `year` from (`fiberdb`.`estates` `e` join ((`fiberdb`.`estate_owners` `eo` join `fiberdb`.`estatebi` `eb`) join `fiberdb`.`estateact` `ea`) on(((`eo`.`estateid` = `e`.`estateid`) and (`eb`.`estateid` = `e`.`estateid`) and (`ea`.`estateid` = `e`.`estateid`)))) where (`eb`.`year` = `ea`.`year`);

-- ----------------------------------------------------------------------------
-- View fiberdb.estatedc_consum
-- ----------------------------------------------------------------------------
USE `fiberdb`;
CREATE  OR REPLACE VIEW `fiberdb`.`estatedc_consum` AS select `estatedc_sum`.`sumamount` AS `conamount`,`estatedc_sum`.`sumtax` AS `contax`,`estatedc_sum`.`estateid` AS `estateid`,`estatedc_sum`.`year` AS `year`,`estatedc_sum`.`months` AS `conmonths` from `fiberdb`.`estatedc_sum` where (`estatedc_sum`.`type` = 'connection-fee');

-- ----------------------------------------------------------------------------
-- View fiberdb.estatedc_opsum
-- ----------------------------------------------------------------------------
USE `fiberdb`;
CREATE  OR REPLACE VIEW `fiberdb`.`estatedc_opsum` AS select `estatedc_sum`.`sumamount` AS `opamount`,`estatedc_sum`.`sumtax` AS `optax`,`estatedc_sum`.`estateid` AS `estateid`,`estatedc_sum`.`year` AS `year`,`estatedc_sum`.`months` AS `opmonths` from `fiberdb`.`estatedc_sum` where (`estatedc_sum`.`type` = 'operator-fee');

-- ----------------------------------------------------------------------------
-- View fiberdb.estatedc_paysum
-- ----------------------------------------------------------------------------
USE `fiberdb`;
CREATE  OR REPLACE VIEW `fiberdb`.`estatedc_paysum` AS select `estatedc_sum`.`sumamount` AS `payamount`,`estatedc_sum`.`estateid` AS `estateid`,`estatedc_sum`.`year` AS `year` from `fiberdb`.`estatedc_sum` where (`estatedc_sum`.`type` = 'payment');

-- ----------------------------------------------------------------------------
-- View fiberdb.estatedc_sum
-- ----------------------------------------------------------------------------
USE `fiberdb`;
CREATE  OR REPLACE VIEW `fiberdb`.`estatedc_sum` AS select ifnull(sum(`fiberdb`.`estatedcs`.`amount`),0) AS `sumamount`,ifnull(sum(`fiberdb`.`estatedcs`.`tax`),0) AS `sumtax`,`fiberdb`.`estatedcs`.`estateid` AS `estateid`,`fiberdb`.`estatedcs`.`type` AS `type`,`fiberdb`.`estatedcs`.`year` AS `year`,`fiberdb`.`estatedcs`.`months` AS `months` from `fiberdb`.`estatedcs` group by `fiberdb`.`estatedcs`.`estateid`,`fiberdb`.`estatedcs`.`type`,`fiberdb`.`estatedcs`.`year`;

-- ----------------------------------------------------------------------------
-- View fiberdb.estatedc_sums
-- ----------------------------------------------------------------------------
USE `fiberdb`;
CREATE  OR REPLACE VIEW `fiberdb`.`estatedc_sums` AS select ifnull(`con`.`conamount`,0) AS `conamount`,ifnull(`con`.`contax`,0) AS `contax`,ifnull(`con`.`conmonths`,0) AS `conmonths`,ifnull(`op`.`opamount`,0) AS `opamount`,ifnull(`op`.`optax`,0) AS `optax`,ifnull(`op`.`opmonths`,0) AS `opmonths`,ifnull(`ep`.`payamount`,0) AS `payamount`,`con`.`estateid` AS `estateid`,`con`.`year` AS `year`,((((ifnull(`con`.`conamount`,0) + ifnull(`con`.`contax`,0)) + ifnull(`op`.`opamount`,0)) + ifnull(`op`.`optax`,0)) + ifnull(`ep`.`payamount`,0)) AS `total` from ((`fiberdb`.`estatedc_consum` `con` left join `fiberdb`.`estatedc_opsum` `op` on(((`op`.`estateid` = `con`.`estateid`) and (`op`.`year` = `con`.`year`)))) left join `fiberdb`.`estatedc_paysum` `ep` on(((`ep`.`estateid` = `con`.`estateid`) and (`ep`.`year` = `con`.`year`)))) group by `con`.`estateid`,`con`.`year`;

-- ----------------------------------------------------------------------------
-- View fiberdb.member_contact_sum
-- ----------------------------------------------------------------------------
USE `fiberdb`;
CREATE  OR REPLACE VIEW `fiberdb`.`member_contact_sum` AS select `mp`.`name` AS `name`,`mp`.`contact` AS `contact`,`mp`.`fromyear` AS `fromyear`,`mp`.`toyear` AS `toyear`,`mdcs`.`memberid` AS `memberid`,`mdcs`.`year` AS `year`,`mdcs`.`amount` AS `amount`,`mdcs`.`tax` AS `tax`,`mdcs`.`total` AS `total` from (`fiberdb`.`member_pref` `mp` join `fiberdb`.`memberdc_sum` `mdcs` on((`mdcs`.`memberid` = `mp`.`memberid`)));

-- ----------------------------------------------------------------------------
-- View fiberdb.member_pref
-- ----------------------------------------------------------------------------
USE `fiberdb`;
CREATE  OR REPLACE VIEW `fiberdb`.`member_pref` AS select `m`.`memberid` AS `memberid`,`m`.`name` AS `name`,`m`.`note` AS `note`,`c`.`value` AS `contact`,`m`.`fromyear` AS `fromyear`,`m`.`toyear` AS `toyear` from (`fiberdb`.`members` `m` join `fiberdb`.`contacts` `c` on((`c`.`memberid` = `m`.`memberid`))) where (`c`.`preferred` = 1);

-- ----------------------------------------------------------------------------
-- View fiberdb.memberdc_sum
-- ----------------------------------------------------------------------------
USE `fiberdb`;
CREATE  OR REPLACE VIEW `fiberdb`.`memberdc_sum` AS select `fiberdb`.`memberdcs`.`memberid` AS `memberid`,`fiberdb`.`memberdcs`.`year` AS `year`,sum(`fiberdb`.`memberdcs`.`amount`) AS `amount`,sum(`fiberdb`.`memberdcs`.`tax`) AS `tax`,sum((`fiberdb`.`memberdcs`.`amount` + `fiberdb`.`memberdcs`.`tax`)) AS `total` from `fiberdb`.`memberdcs` group by `fiberdb`.`memberdcs`.`memberid`,`fiberdb`.`memberdcs`.`year`;

-- ----------------------------------------------------------------------------
-- View fiberdb.memberpref
-- ----------------------------------------------------------------------------
USE `fiberdb`;
CREATE  OR REPLACE VIEW `fiberdb`.`memberpref` AS select `m`.`memberid` AS `memberid`,`m`.`name` AS `name`,`c`.`value` AS `contact`,`m`.`fromyear` AS `fromyear`,`m`.`toyear` AS `toyear` from (`fiberdb`.`members` `m` join `fiberdb`.`contacts` `c`) where ((`c`.`memberid` = `m`.`memberid`) and (`c`.`preferred` = 1));

SET FOREIGN_KEY_CHECKS = 1;
