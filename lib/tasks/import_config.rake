desc "Import configs from Excel to mysql"
task :import_config => :environment do
  # Clean all the old config migrations
  Rails.env = 'production'
  Dir.glob("#{Rails.root.to_s}/db/migrate/*").each do |path|
    if path =~ /_create_config_/
      puts "path:#{path}"
      version = File.basename(path, ".rb").split('_').first
      ENV['VERSION'] = version
      Rake::Task["db:migrate:down"].execute()
      `rm #{path}`
    end
  end

  ENV.delete('VERSION')

  # Clean all the old config models
  Dir.glob("#{Rails.root.to_s}/app/models/*").each do |path|
    `rm #{path}` if path =~ /config_/
  end

  Rails.env = 'config'
  Rake::Task['db:drop'].execute
  Rake::Task['db:create'].execute

  #s = Roo::Excelx.new("#{Rails.root.to_s}/config/game_data/example.xlsx")
  s = Roo::Excel.new("#{Rails.root.to_s}/config/game_data/item_config.xls")
  s.sheets.each do |sheet|
    s.default_sheet = sheet
    fields = s.first
    field_types = []
    field_names = []
    fields_define = fields.map do |field|
      name, type = field.split(":")
      field_names << name
      field_types << type
      "#{name}:#{type}"
    end.join(" ")

    `cd #{Rails.root.to_s} && rails g model #{sheet.singularize} #{fields_define}`
    Rake::Task['db:migrate'].execute

    field_names = field_names.map do |field_name|
      "`#{field_name}`"
    end.join(',')

    values = 2.upto(s.last_row).map do |row|
      row_values = []
      s.row(row).each_with_index do |value, index|
        value = ActiveRecord::Base.sanitize(value) if field_types[index] == 'string'
        value = 0 if field_types[index] == 'integer' and value.blank?
        value = 0.0 if field_types[index] == 'float' and value.blank?
        row_values << value
      end
      "(#{row_values.join(',')})"
    end.join(',')

    ActiveRecord::Base.connection.execute(
      "INSERT INTO `#{sheet}` (#{field_names}) VALUES #{values}"
    )
  end
end
