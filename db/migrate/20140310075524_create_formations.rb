class CreateFormations < ActiveRecord::Migration
  def change
    create_table :formations, id: false do |t|
      t.string :uuid
      t.string :user_id
      t.string :matrix
    end

    add_index :formations, :uuid, unique: true
    add_index :formations, :user_id
  end
end
