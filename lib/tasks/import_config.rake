desc "Import configs from Excel to mysql"
task :import_config => :environment do
  Rails.env = 'production'
end
