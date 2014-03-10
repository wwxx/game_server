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

ActiveRecord::Schema.define(:version => 20140121055124) do

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

  create_table "config_players", :force => true do |t|
    t.integer "level"
    t.float   "exp"
    t.string  "title"
    t.text    "title_desc"
  end

  create_table "config_stores", :force => true do |t|
    t.string  "name"
    t.integer "amount"
  end

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
