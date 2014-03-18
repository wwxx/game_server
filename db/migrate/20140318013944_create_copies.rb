class CreateCopies < ActiveRecord::Migration
  def change
    create_table :copies, id: false do |t|
      t.string :uuid
      t.string :user_id
      t.integer :area_id
      t.integer :copy_id
      t.integer :star
      t.integer :battle_process
      t.boolean :is_rewarded
    end

    add_index :copies, :uuid, unique: true
    add_index :copies, :user_id
    add_index :copies, :area_id
    add_index :copies, :copy_id
  end
end
