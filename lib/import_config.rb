require 'bundler/setup'
require 'erlectricity'
require 'Roo'

s = Roo::Excelx.new(File.expand_path("./config/game_data/example.xlsx"))

receive do |f|
  f.when([:import_config, String]) do |_text|
    s.sheets.each do |sheet|
      table_name = sheet.to_sym
      f.send!([:create_table, table_name])
      s.default_sheet = sheet
      fields = s.first
      rows = s.last_row
      columns = s.last_column
      2.upto(rows) do |row|
        puts "row: #{s.row(row).inspect}"
        f.send!([:insert, table_name, s.row(row)])
        # f.send!([:insert, table_name, ["a", "b"]])
      end
    end
    f.send!(:finish)
  end
end
