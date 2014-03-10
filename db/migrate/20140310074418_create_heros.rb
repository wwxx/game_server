class CreateHeros < ActiveRecord::Migration
  def change
    create_table :heros, id: false do |t|
      t.string :uuid
      t.string :user_id
      t.integer :level
      t.integer :config_id
      t.timestamps
    end

    add_index :heros, :uuid, unique: true
    add_index :heros, :user_id
  end
end
