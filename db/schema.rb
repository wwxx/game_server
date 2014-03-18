# encoding: UTF-8
# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended to check this file into your version control system.

ActiveRecord::Schema.define(:version => 20140318013944) do

  create_table "config_alliance_attacks", :force => true do |t|
    t.integer "level"
    t.integer "max_amount"
  end

  create_table "config_alliance_defenses", :force => true do |t|
    t.integer "level"
    t.integer "max_amount"
  end

  create_table "config_armies", :force => true do |t|
    t.string  "army_name"
    t.integer "barrack_level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
    t.integer "gold_cure"
    t.float   "gem_cure"
    t.integer "attack"
    t.integer "defence"
    t.integer "health"
    t.integer "accurate"
    t.integer "dodge"
    t.integer "crit"
    t.integer "tenacity"
    t.integer "load"
  end

  create_table "config_army_controls", :force => true do |t|
    t.integer "hero_level"
    t.integer "aptitude_1"
    t.integer "aptitude_2"
    t.integer "aptitude_3"
    t.integer "aptitude_4"
    t.integer "aptitude_5"
    t.integer "aptitude_6"
  end

  create_table "config_building_academies", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_building_barracks", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_building_bed_positions", :force => true do |t|
    t.integer "position"
    t.integer "x"
    t.integer "y"
    t.integer "width"
    t.integer "height"
  end

  create_table "config_building_castles", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_building_embassies", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_building_farms", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_building_hidden_warehous", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_building_hospitals", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_building_houses", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_building_markets", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_building_ores", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_building_quarries", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_building_sawmills", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_building_war_centers", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_building_warehouses", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_building_watch_towers", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
    t.integer "duration"
  end

  create_table "config_commander_leads", :force => true do |t|
    t.integer "level"
    t.integer "army_control"
    t.integer "book_amount"
    t.integer "rate"
  end

  create_table "config_commander_prestiges", :force => true do |t|
    t.integer "level"
    t.integer "exp"
  end

  create_table "config_commanders", :force => true do |t|
    t.integer "level"
    t.integer "exp"
    t.integer "army_control"
  end

  create_table "config_hidden_warehouses", :force => true do |t|
    t.integer "level"
    t.integer "storage"
  end

  create_table "config_hospitals", :force => true do |t|
    t.integer "level"
    t.integer "max_patients"
    t.integer "cure_time"
  end

  create_table "config_items", :force => true do |t|
    t.text    "title"
    t.string  "ver"
    t.string  "obj"
    t.string  "number"
    t.integer "no"
    t.string  "url"
    t.string  "pc"
    t.string  "ken"
  end

  create_table "config_markets", :force => true do |t|
    t.integer "level"
    t.integer "amount"
    t.integer "tax"
  end

  create_table "config_player_grows", :force => true do |t|
    t.integer "config_id"
    t.string  "name"
    t.string  "name_desc"
    t.integer "health"
    t.integer "attack"
    t.integer "defense"
    t.integer "recover"
    t.integer "skill_power"
    t.integer "crit"
    t.integer "crit_damage"
    t.integer "accurate"
    t.integer "dodge"
    t.integer "parry"
    t.integer "block"
  end

  create_table "config_players", :force => true do |t|
    t.integer "config_id"
    t.string  "name"
    t.string  "name_desc"
    t.integer "profession"
    t.integer "skill_id"
    t.integer "attack_type"
    t.integer "rare"
    t.integer "health"
    t.integer "attack"
    t.integer "defense"
    t.integer "recover"
    t.integer "skill_power"
    t.integer "crit"
    t.integer "crit_damage"
    t.integer "accurate"
    t.integer "dodge"
    t.integer "parry"
    t.integer "block"
  end

  create_table "config_populations", :force => true do |t|
    t.integer "house_level"
    t.integer "max_population"
    t.integer "recover_per_hour"
    t.integer "gold_generate"
  end

  create_table "config_resource_generates", :force => true do |t|
    t.integer "level"
    t.integer "food"
    t.integer "wood"
    t.integer "stone"
    t.integer "ore"
  end

  create_table "config_stores", :force => true do |t|
    t.string  "name"
    t.integer "amount"
  end

  create_table "config_training_limits", :force => true do |t|
    t.integer "barrack_level"
    t.integer "max_training"
  end

  create_table "config_unlock_buildings", :force => true do |t|
    t.string "building_name"
    t.string "unlock_condition"
  end

  create_table "config_users", :force => true do |t|
    t.integer "level"
    t.float   "exp"
    t.string  "title"
    t.text    "title_desc"
  end

  create_table "config_warehouses", :force => true do |t|
    t.integer "level"
    t.integer "storage"
  end

  create_table "copies", :id => false, :force => true do |t|
    t.string  "uuid"
    t.string  "user_id"
    t.integer "copy_id"
    t.integer "star"
    t.integer "battle_process"
    t.boolean "is_rewarded"
  end

  add_index "copies", ["copy_id"], :name => "index_copies_on_copy_id"
  add_index "copies", ["user_id"], :name => "index_copies_on_user_id"
  add_index "copies", ["uuid"], :name => "index_copies_on_uuid", :unique => true

  create_table "formations", :id => false, :force => true do |t|
    t.string "uuid"
    t.string "user_id"
    t.string "matrix"
  end

  add_index "formations", ["user_id"], :name => "index_formations_on_user_id"
  add_index "formations", ["uuid"], :name => "index_formations_on_uuid", :unique => true

  create_table "heros", :id => false, :force => true do |t|
    t.string   "uuid"
    t.string   "user_id"
    t.integer  "level"
    t.integer  "config_id"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "heros", ["user_id"], :name => "index_heros_on_user_id"
  add_index "heros", ["uuid"], :name => "index_heros_on_uuid", :unique => true

  create_table "towns", :id => false, :force => true do |t|
    t.string   "uuid"
    t.string   "user_id"
    t.string   "name"
    t.integer  "x"
    t.integer  "y"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "towns", ["uuid"], :name => "index_towns_on_uuid", :unique => true

  create_table "users", :id => false, :force => true do |t|
    t.string   "uuid"
    t.string   "udid"
    t.string   "name"
    t.integer  "gem"
    t.float    "paid"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "users", ["name"], :name => "index_users_on_name", :unique => true
  add_index "users", ["udid"], :name => "index_users_on_udid", :unique => true
  add_index "users", ["uuid"], :name => "index_users_on_uuid", :unique => true

end
