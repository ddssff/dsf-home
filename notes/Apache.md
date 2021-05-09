# To add a new site:

For example we have deb.seereason.com, we want a similar site deb8.seereason.com.  Copy /etc/apache2/sites-available/deb.seereason.com.conf and make desired changes.  Then

sudo a2ensite deb8.seereason.com.conf
Mkdir /srv/deb8
Mkdir /var/log/apache2/deb8.seereason.com
sudo service apache restart
