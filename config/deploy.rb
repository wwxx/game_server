@target = :dev

@servers = {
}

@dev_server = "115.29.14.9"

set :application,   "erlangService"
#set :repository,    "git@115.29.14.9:/home/git/server_001.git"
set :repository,    "git@github.com:mafei198/game_server.git"
set :scm,           :git
set :keep_releases, 5
set :deploy_to,     "/home/ubuntu/www/game_server"
set :runner,        "ubuntu"
set :branch,        @git_branch
set :use_sudo,      false
set :user,          "ubuntu"
set :deploy_via,    :remote_cache

set :default_environment, {
    'PATH' => "/usr/local/bin:${PATH}"
}

ssh_options[:forward_agent] = true
ssh_options[:paranoid]      = true
default_run_options[:pty]   = true

if @target == :all
  @web_servers = @servers.values
  @app_servers = @servers.values
  @db_servers  = @servers.values
elsif @target == :dev
  @web_servers = [@dev_server]
  @app_servers = [@dev_server]
  @db_servers  = [@dev_server]
else
  puts "No Such Target!!!"
end

puts "@web_servers:#{@web_servers}"
puts "@app_servers:#{@app_servers}"
puts "@db_servers:#{@db_servers}"

role :web, *@web_servers
role :app, *@app_servers
role :db,  *@db_servers, primary: true

after "deploy:restart", "deploy:cleanup"

after "deploy:create_symlink", "deploy:install"

namespace :deploy do
 task :install do
   ruby "cd #{current_path} && rebar clean && rebar compile"
 end

 task :setup_server do
   run "cd #{current_path} && ./install.sh"
 end

 task :start do
   run "cd #{current_path} && ./start"
 end

 task :stop do
   run "cd #{current_path} && ./stop"
 end

 #task :restart, :roles => :app, :except => { :no_release => true } do
 #end
end

def run_silent(command)
  begin
    run command
  rescue => e
    puts '**********Exception from run_silent***********'
    puts e
    puts '**********************************************'
  end
end
