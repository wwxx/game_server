
        DROP TABLE IF EXISTS `config_players`;
        CREATE TABLE `config_players` (
          `id` int(11) NOT NULL AUTO_INCREMENT,
          `config_id` int(11) DEFAULT NULL,
`name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
`name_desc` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
`profession` int(11) DEFAULT NULL,
`skill_id` int(11) DEFAULT NULL,
`attack_type` int(11) DEFAULT NULL,
`rare` int(11) DEFAULT NULL,
`health` int(11) DEFAULT NULL,
`attack` int(11) DEFAULT NULL,
`defense` int(11) DEFAULT NULL,
`recover` int(11) DEFAULT NULL,
`skill_power` int(11) DEFAULT NULL,
`crit` int(11) DEFAULT NULL,
`crit_damage` int(11) DEFAULT NULL,
`accurate` int(11) DEFAULT NULL,
`dodge` int(11) DEFAULT NULL,
`parry` int(11) DEFAULT NULL,
`block` int(11) DEFAULT NULL,
          PRIMARY KEY (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
      INSERT INTO `config_players` (`config_id`,`name`,`name_desc`,`profession`,`skill_id`,`attack_type`,`rare`,`health`,`attack`,`defense`,`recover`,`skill_power`,`crit`,`crit_damage`,`accurate`,`dodge`,`parry`,`block`) VALUES (1.0,'player','主角',21.0,0.0,1.0,1.0,100.0,0.0,80.0,50.0,60.0,60.0,0.0,50.0,0.0,0.0,1000.0),(2.0,'h2','里昂',31.0,0.0,2.0,2.0,20.0,180.0,70.0,70.0,60.0,100.0,0.0,0.0,0.0,0.0,1000.0),(3.0,'h3','未用',20.0,0.0,1.0,2.0,70.0,7.0,40.0,16.0,60.0,56.0,0.0,50.0,0.0,0.0,1000.0),(4.0,'h7','希尔瓦',11.0,0.0,1.0,3.0,90.0,0.0,72.0,40.0,55.0,55.0,0.0,0.0,0.0,0.0,1000.0),(5.0,'h5','奥尼尔',10.0,0.0,2.0,3.0,270.0,0.0,0.0,80.0,70.0,90.0,60.0,25.0,200.0,0.0,1000.0),(6.0,'h6','冈萨罗',40.0,0.0,3.0,4.0,380.0,0.0,100.0,50.0,80.0,60.0,0.0,0.0,0.0,0.0,1000.0),(7.0,'h7','莱科宁',41.0,0.0,2.0,5.0,220.0,180.0,70.0,70.0,60.0,100.0,0.0,0.0,0.0,0.0,1000.0),(8.0,'h8','阿尔萨',10.0,0.0,1.0,1.0,550.0,0.0,100.0,50.0,60.0,60.0,0.0,25.0,200.0,0.0,1000.0),(9.0,'h9','斯嘉丽',11.0,0.0,1.0,2.0,570.0,0.0,0.0,80.0,70.0,90.0,60.0,25.0,200.0,0.0,1000.0),(10.0,'h10','多米里安',10.0,0.0,2.0,3.0,780.0,0.0,100.0,50.0,80.0,60.0,0.0,0.0,0.0,0.0,1000.0),(11.0,'h11','卡桑德拉',10.0,0.0,1.0,4.0,1220.0,180.0,70.0,70.0,60.0,100.0,0.0,0.0,0.0,0.0,1000.0);
        DROP TABLE IF EXISTS `config_player_grows`;
        CREATE TABLE `config_player_grows` (
          `id` int(11) NOT NULL AUTO_INCREMENT,
          `config_id` int(11) DEFAULT NULL,
`name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
`name_desc` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
`health` int(11) DEFAULT NULL,
`attack` int(11) DEFAULT NULL,
`defense` int(11) DEFAULT NULL,
`recover` int(11) DEFAULT NULL,
`skill_power` int(11) DEFAULT NULL,
`crit` int(11) DEFAULT NULL,
`crit_damage` int(11) DEFAULT NULL,
`accurate` int(11) DEFAULT NULL,
`dodge` int(11) DEFAULT NULL,
`parry` int(11) DEFAULT NULL,
`block` int(11) DEFAULT NULL,
          PRIMARY KEY (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
      INSERT INTO `config_player_grows` (`config_id`,`name`,`name_desc`,`health`,`attack`,`defense`,`recover`,`skill_power`,`crit`,`crit_damage`,`accurate`,`dodge`,`parry`,`block`) VALUES (1.0,'player','主角',100.0,0.0,80.0,50.0,60.0,60.0,0.0,50.0,0.0,0.0,1000.0),(2.0,'h2','里昂',20.0,180.0,70.0,70.0,60.0,100.0,0.0,0.0,0.0,0.0,1000.0),(3.0,'h3','未用',70.0,7.0,40.0,16.0,60.0,56.0,0.0,50.0,0.0,0.0,1000.0),(4.0,'h7','希尔瓦',90.0,0.0,72.0,40.0,55.0,55.0,0.0,0.0,0.0,0.0,1000.0),(5.0,'h5','奥尼尔',270.0,0.0,0.0,80.0,70.0,90.0,60.0,25.0,200.0,0.0,1000.0),(6.0,'h6','冈萨罗',380.0,0.0,100.0,50.0,80.0,60.0,0.0,0.0,0.0,0.0,1000.0),(7.0,'h7','莱科宁',220.0,180.0,70.0,70.0,60.0,100.0,0.0,0.0,0.0,0.0,1000.0),(8.0,'h8','阿尔萨',550.0,0.0,100.0,50.0,60.0,60.0,0.0,25.0,200.0,0.0,1000.0),(9.0,'h9','斯嘉丽',570.0,0.0,0.0,80.0,70.0,90.0,60.0,25.0,200.0,0.0,1000.0),(10.0,'h10','多米里安',780.0,0.0,100.0,50.0,80.0,60.0,0.0,0.0,0.0,0.0,1000.0),(11.0,'h11','卡桑德拉',1220.0,180.0,70.0,70.0,60.0,100.0,0.0,0.0,0.0,0.0,1000.0);