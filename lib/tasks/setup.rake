task :setup => :environment do
  puts ">>>>>>>>> Create Database"
  Rake::Task['db:create'].execute
  puts ""

  puts ">>>>>>>>> Migrate Database"
  Rake::Task['db:migrate'].execute
  puts ""

  puts ">>>>>>>>> Generate Configs"
  Rake::Task['generate_config'].execute
  puts ""

  puts ">>>>>>>>> Generate Record"
  Rake::Task['generate_record'].execute
  puts ""
end
