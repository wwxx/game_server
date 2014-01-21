class CreateTowns < ActiveRecord::Migration
  def change
    create_table :towns, id: false do |t|
      t.string :uuid
      t.string :user_id
      t.string :name
      t.integer :x
      t.integer :y

      t.timestamps
    end

    add_index :towns, :uuid, unique: true
  end
end
