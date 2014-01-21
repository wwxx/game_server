class CreateConfigItems < ActiveRecord::Migration
  def change
    create_table :config_items do |t|
      t.string :title
      t.string :ver
      t.string :obj
      t.string :number
      t.integer :no
      t.string :url
      t.string :pc
      t.string :ken

      t.timestamps
    end
  end
end
