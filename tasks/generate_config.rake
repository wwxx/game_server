## The MIT License (MIT)
##
## Copyright (c) 2014-2024
## Savin Max <mafei.198@gmail.com>
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.


desc "Generate configs Sql format file from Excel"
task :generate_config => :environment do
  config_dir = File.expand_path("#{FRAMEWORK_ROOT_DIR}/app/config_data/game_data")
  sheets = []

  Dir.foreach(config_dir) do |config_file_path|
    extname = File.extname(config_file_path)
    if extname == '.xlsx'
      s = Roo::Excelx.new(File.expand_path(config_dir + '/' + config_file_path))
    elsif extname == '.xls'
      s = Roo::Excel.new(File.expand_path(config_dir + '/'+ config_file_path))
    else
      next
    end

    sql = ""

    sheets += s.sheets

    s.sheets.each do |sheet|
      s.default_sheet = sheet
      table_name = sheet.pluralize
      File.open("#{Rails.root.to_s}/app/models/#{sheet.singularize}.rb", 'w') do |io|
        model_name = sheet.singularize.camelize
        io.write "# Generated by generate_config.rake #{Time.now}\n"
        io.write "class #{model_name} < ActiveRecord::Base;end"
      end
      fields_define = []
      field_types = []
      field_names = []
      s.row(2).each do |field|
        name, type = field.split(":")
        field_names << name
        field_types << type
        case type
        when 'string'
          fields_define << "`#{name}` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL"
        when 'text'
          fields_define << "`#{name}` text COLLATE utf8_unicode_ci"
        when 'integer'
          fields_define << "`#{name}` int(11) DEFAULT NULL"
        when 'float'
          fields_define << "`#{name}` float DEFAULT NULL"
        else
          raise "TYPE ERROR: #{type} didn't defined."
        end
      end
      sql << %Q{
        DROP TABLE IF EXISTS `#{table_name}`;
        CREATE TABLE `#{table_name}` (
          `id` int(11) NOT NULL AUTO_INCREMENT,
          #{fields_define.join(",\n")},
          PRIMARY KEY (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
      }
      values = 3.upto(s.last_row).map do |row|
        row_values = []
        s.row(row).each_with_index do |value, index|
          if ['string', 'text'].include?(field_types[index])
            value = ActiveRecord::Base.sanitize(value)
          end
          value = 0 if field_types[index] == 'integer' and value.blank?
          value = 0.0 if field_types[index] == 'float' and value.blank?
          row_values << value
        end
        "(#{row_values.join(',')})"
      end.join(',')

      field_names = field_names.map do |field_name|
        "`#{field_name}`"
      end.join(',')

      sql << "INSERT INTO `#{table_name}` (#{field_names}) VALUES #{values};"
      file_path = "#{Rails.root.to_s}/db/config_data.sql"
      File.open(file_path, "w") do |io|
        io.write sql
      end
      database_name = Rails.configuration.database_configuration[Rails.env]["database"]

      # Make sure mysql path is: '/usr/bin/mysql'
      `mysql -u root #{database_name} < #{file_path}`
    end

    File.open("#{FRAMEWORK_ROOT_DIR}/game_server/include/config_names.hrl", 'w') do |io|
      io.puts "%%% Generated by generate_config.rake #{Time.now}\n"
      io.puts "-define(CONFIG_MODELS, [\n"
      io.puts "        #{sheets.map(&:pluralize).join(",\n        ")}"
      io.puts "        ])."
    end
  end
end