desc "Import configs from Excel to mysql"
task :import_config => :environment do
  Rails.env = 'production'

  s = Roo::Excelx.new("#{Rails.root.to_s}/game_data/config.xlsx")
end
