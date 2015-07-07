Dickminer
====

This command line utility is able to parse the asciidick.com logs and aims to provide *analytic* value to the project.

Current interaction:

```
creek@ubuntu:~/sandbox/dickminer$ .stack-work/dist/x86_64-linux/Cabal-1.18.1.5/build/dickminer-exe/dickminer-exe data/traffic.log
Raw Data:
[Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:46:28 UTC}),Nothing,Just (Pagehit {ip = "
130.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:46:58 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", 
city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:08 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country =
 "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:10 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55
.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:12 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, ti
mestamp = 2015-07-06 08:47:14 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:15 
UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:16 UTC}),Just (Pagehit {ip = "130
.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:17 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", cit
y = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:25 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "D
ENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:25 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.66
67, longitude = 12.5667, timestamp = 2015-07-06 08:47:26 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, times
tamp = 2015-07-06 08:47:27 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:28 UTC
}),Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:29 UTC}),Just (Pagehit {ip = "130.22
6.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:30 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", city =
 "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:31 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "DENM
ARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:31 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667,
 longitude = 12.5667, timestamp = 2015-07-06 08:47:32 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestam
p = 2015-07-06 08:47:33 UTC}),Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:34 UTC}),
Just (Pagehit {ip = "130.226.142.243", page = "/", city = "Copenhagen", country = "DENMARK (DK)", latitude = 55.6667, longitude = 12.5667, timestamp = 2015-07-06 08:47:35 UTC}),Just (Pagehit {ip = "71.227.13
.74", page = "/?length=1&diameter=10&ball_radius=100", city = "Redmond, WA", country = "UNITED STATES (US)", latitude = 47.6742, longitude = -122.115, timestamp = 2015-07-06 08:50:19 UTC}),Just (Pagehit {ip 
= "71.227.13.74", page = "/?length=1&diameter=10&ball_radius=10", city = "Redmond, WA", country = "UNITED STATES (US)", latitude = 47.6742, longitude = -122.115, timestamp = 2015-07-06 08:50:23 UTC}),Just (P
agehit {ip = "71.227.13.74", page = "/?length=10&diameter=10&ball_radius=10", city = "Redmond, WA", country = "UNITED STATES (US)", latitude = 47.6742, longitude = -122.115, timestamp = 2015-07-06 08:50:41 U
TC}),Just (Pagehit {ip = "71.227.13.74", page = "/?length=20&diameter=10&ball_radius=10", city = "Redmond, WA", country = "UNITED STATES (US)", latitude = 47.6742, longitude = -122.115, timestamp = 2015-07-0
6 08:50:46 UTC}),Just (Pagehit {ip = "71.227.13.74", page = "/?length=30&diameter=10&ball_radius=10", city = "Redmond, WA", country = "UNITED STATES (US)", latitude = 47.6742, longitude = -122.115, timestamp
 = 2015-07-06 08:50:50 UTC}),Just (Pagehit {ip = "71.227.13.74", page = "/?length=30&diameter=20&ball_radius=10", city = "Redmond, WA", country = "UNITED STATES (US)", latitude = 47.6742, longitude = -122.11
5, timestamp = 2015-07-06 08:50:54 UTC}),Just (Pagehit {ip = "71.227.13.74", page = "/?length=30&diameter=30&ball_radius=10", city = "Redmond, WA", country = "UNITED STATES (US)", latitude = 47.6742, longitu
de = -122.115, timestamp = 2015-07-06 08:51:03 UTC}),Just (Pagehit {ip = "71.227.13.74", page = "/?length=30&diameter=30&ball_radius=30", city = "Redmond, WA", country = "UNITED STATES (US)", latitude = 47.6
742, longitude = -122.115, timestamp = 2015-07-06 08:51:08 UTC}),Just (Pagehit {ip = "71.227.13.74", page = "/?length=30&diameter=30&ball_radius=20", city = "Redmond, WA", country = "UNITED STATES (US)", lat
itude = 47.6742, longitude = -122.115, timestamp = 2015-07-06 08:51:12 UTC}),Just (Pagehit {ip = "71.227.13.74", page = "/?length=30&diameter=30&ball_radius=10", city = "Redmond, WA", country = "UNITED STATE
S (US)", latitude = 47.6742, longitude = -122.115, timestamp = 2015-07-06 08:51:17 UTC}),Just (Pagehit {ip = "71.227.13.74", page = "/?length=30&diameter=25&ball_radius=10", city = "Redmond, WA", country = "
UNITED STATES (US)", latitude = 47.6742, longitude = -122.115, timestamp = 2015-07-06 08:51:23 UTC}),Just (Pagehit {ip = "71.227.13.74", page = "/", city = "Redmond, WA", country = "UNITED STATES (US)", lati
tude = 47.6742, longitude = -122.115, timestamp = 2015-07-06 09:03:48 UTC}),Just (Pagehit {ip = "71.227.13.74", page = "/?length=30&diameter=30&ball_radius=10", city = "Redmond, WA", country = "UNITED STATES
 (US)", latitude = 47.6742, longitude = -122.115, timestamp = 2015-07-06 09:03:56 UTC}),Just (Pagehit {ip = "71.227.13.74", page = "/?length=30&diameter=20&ball_radius=10", city = "Redmond, WA", country = "U
NITED STATES (US)", latitude = 47.6742, longitude = -122.115, timestamp = 2015-07-06 09:04:02 UTC}),Just (Pagehit {ip = "71.227.13.74", page = "/?length=25&diameter=20&ball_radius=10", city = "Redmond, WA", 
country = "UNITED STATES (US)", latitude = 47.6742, longitude = -122.115, timestamp = 2015-07-06 09:04:08 UTC}),Just (Pagehit {ip = "71.227.13.74", page = "/?length=25&diameter=20&ball_radius=7", city = "Red
mond, WA", country = "UNITED STATES (US)", latitude = 47.6742, longitude = -122.115, timestamp = 2015-07-06 09:04:13 UTC}),Just (Pagehit {ip = "71.227.13.74", page = "/", city = "Redmond, WA", country = "UNI
TED STATES (US)", latitude = 47.6742, longitude = -122.115, timestamp = 2015-07-06 09:16:39 UTC}),Just (Pagehit {ip = "71.227.13.74", page = "/", city = "Redmond, WA", country = "UNITED STATES (US)", latitud
e = 47.6742, longitude = -122.115, timestamp = 2015-07-06 09:16:42 UTC})]                                                                                                                                     
=========================
There were: 41 victims.
=========================
We hit these cities: ["Copenhagen","Redmond, WA"].

```


