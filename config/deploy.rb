@target = :dev

@servers = {
  g001: "106.186.20.50",
  cn001: "121.199.57.143"
}

@dev_server = "42.96.138.143"

set :application,   "erlangService"
set :repository,    "git@42.96.138.143:/data/git/erlangServer.git"
set :scm,           :git
set :keep_releases, 5
set :deploy_to,     "/home/ubuntu/www/erlangService"
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
   run "cd #{current_path}/pusher && rebar clean && ./install.sh && rebar generate"
 end

 task :start do
   run_silent "cd #{current_path}/pusher && ./rel/pusher/bin/pusher start"
 end

 task :stop do
   run_silent "cd #{current_path}/pusher && ./rel/pusher/bin/pusher stop"
 end

 task :restart, :roles => :app, :except => { :no_release => true } do
   #run_silent "#{current_path}/pusher/rel/pusher/bin/pusher stop"
   #run_silent "#{current_path}/pusher/rel/pusher/bin/pusher start"
   #run_silent "#{current_path}/pusher/rel/pusher/bin/pusher restart"
 end
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
