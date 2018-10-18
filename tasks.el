(defvar load-django-environment ""
  "pre-run hook to initialize environment before running django tasks")

(cyanide-task
 :id 'terminal-in-project
 :display-name "terminal"
 :description "launch terminal in project"
 :func (lambda ()
         (interactive)
         (let ((default-directory (cyanide-project-oref :path)))
           (call-process-shell-command
            terminal-command))))

(cyanide-task
 :id 'netstat
 :display-name "netstat -lntp"
 :description "view listening processes on local machine"
 :func (lambda ()
         (interactive)
         (cd-proj-root)
         (let ((default-directory (cyanide-project-oref :path))
               (async-shell-command-buffer 'confirm-kill-process))
           (async-shell-command
            (read-string "Async shell command: "
                         "netstat -lntp")
            "*netstat*"))))

(cyanide-simple-task :id 'npm-test :command "npm test;")

(cyanide-task
 :id 'restart-postgres-dev-docker
 :display-name "restart-postgres-dev-docker.sh"
 :description (concat
               "start the local dev database if it has not yet been started. "
               "If its already running, restart it.")
 :func (lambda ()
         (interactive)
         (cd-proj-root)
         (let ((default-directory (cyanide-project-oref :path))
               (async-shell-command-buffer 'confirm-kill-process))
           (async-shell-command
            (read-string
             "Async shell command: "
             "scripts/restart-postgres-dev-docker.sh")
            "*restart-postgres-dev-docker.sh*"))))

(cyanide-task
 :id 'restart-redis-dev-docker
 :display-name "restart-redis-dev-docker.sh"
 :description (concat
               "start the local dev database if it has not yet been started. "
               "If its already running, restart it.")
 :func (lambda ()
         (interactive)
         (cd-proj-root)
         (let ((default-directory (cyanide-project-oref :path))
               (async-shell-command-buffer 'confirm-kill-process))
           (async-shell-command
            (read-string
             "Async shell command: "
             "scripts/restart-redis-dev-docker.sh")
            "*restart-redis-dev-docker.sh*"))))

(cyanide-task
 :id 'start-lightmatter-django-site
 :display-name "start.sh"
 :description (concat
               "install dependencies, run model migrations, set up git and "
               "site environment")
 :func (lambda ()
         (interactive)
         (cd-proj-root)
         (let ((default-directory (cyanide-project-oref :path))
               (async-shell-command-buffer 'confirm-kill-process))
           (async-shell-command
            (read-string "Async shell command: "
                         (concat
                          "source scripts/get-env-secrets.sh;"
                          "\n"
                          "source `which virtualenvwrapper.sh`;"
                          "\n"
                          "export ENV_NAME="
                          project-env-name
                          "\n"
                          "export DJANGO_SETTINGS_MODULE=$ENV_NAME.$ENV_NAME.settings.local;"
                          "\n"
                          "scripts/start.sh"))
            "*start.sh*"))))

(cyanide-task
 :id 'kill-lightmatter-django-site
 :display-name "stop.sh"
 :description "stop local twochairs dev site"
 :func (lambda ()
         (interactive)
         (cd-proj-root)
         (let ((default-directory (cyanide-project-oref :path))
               (async-shell-command-buffer 'confirm-kill-process))
           (async-shell-command
            (read-string "Async shell command: " "scripts/stop.sh")
            "*stop.sh*"))))

(cyanide-task
 :id 'manage-dot-py-create-superuser
 :display-name "create superuser"
 :description "create a django superuser for the site"
 :func (lambda ()
         (interactive)
         (cd-proj-root)
         (let ((default-directory (cyanide-project-oref :path))
               (async-shell-command-buffer 'confirm-kill-process))
           (async-shell-command
            (read-string "Async shell command: "
                         (concat
                          load-django-environment
                          "\n"
                          "./manage.py createsuperuser;"))
            "*create superuser*"))))

(cyanide-task
 :id 'manage-dot-py-makemigrations
 :display-name "makemigrations"
 :description "generate migrations after changes to models"
 :func (lambda ()
         (interactive)
         (cd-proj-root)
         (let ((default-directory (cyanide-project-oref :path))
               (async-shell-command-buffer 'confirm-kill-process))
           (async-shell-command
            (read-string "Async shell command: "
                         (concat
                          load-django-environment
                          "\n"
                          "./manage.py makemigrations;"))
            "*make migrations*"))))

(cyanide-task
 :id 'manage-dot-py-migrate
 :display-name "migrate"
 :description "apply new migrations, if they exist"
 :func (lambda ()
         (interactive)
         (cd-proj-root)
         (let ((default-directory (cyanide-project-oref :path))
               (async-shell-command-buffer 'confirm-kill-process))
           (async-shell-command
            (read-string "Async shell command: "
                         (concat
                          load-django-environment
                          "\n"
                          "./manage.py migrate;"))
            "*migrate*"))))

(cyanide-task
 :id 'ipython-in-project-root
 :display-name "ipython"
 :description "launch ipython inside site environment"
 :func (lambda ()
         (interactive)
         (let ((default-directory (cyanide-project-oref :path))
               (async-shell-command-buffer 'confirm-kill-process))
           (async-shell-command
            (read-string "Async shell command: "
                         (concat
                          load-django-environment
                          "\n"
                          "ipython;"))
            "*ipython*"))))

(cyanide-simple-task
 :id 'workon
 :prefix load-django-environment
 :command "bash")

(cyanide-simple-task
 :id 'start-celery
 :command "celery -A twochairs beat -l info"
 :prefix load-django-environment)

(cyanide-simple-task
 :id 'rpdb-connect
 :prefix load-django-environment
 :command "nc localhost 4444 ;")

(cyanide-simple-task
 :id 'manage-dot-py-runserver
 :prefix load-django-environment
 :command "./manage.py runserver")

(cyanide-simple-task
 :id 'npm-run-dev
 :prefix load-django-environment
 :command "npm run dev")

(cyanide-simple-task
 :id 'sudo-systemctl-start-docker
 :command "sudo systemctl start docker")

(defvar django-tasks '(restart-postgres-dev-docker
                       restart-redis-dev-docker
                       start-lightmatter-django-site
                       manage-dot-py-runserver
                       npm-run-dev
                       kill-lightmatter-django-site
                       netstat
                       manage-dot-py-create-superuser
                       manage-dot-py-makemigrations
                       manage-dot-py-migrate
                       ipython-in-project-root
                       terminal-in-project
                       workon
                       start-celery
                       rpdb-connect
                       sudo-systemctl-start-docker)
  "set of tasks to work with generic-django-template")

(provide 'tasks)
