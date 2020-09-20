# saci

A pure Erlang utility library for interfacing with Google Cloud Storage, based on Enenra, jsone and ibrowse.
Redesigned as a utility, it aims to support flexible handling of multiple credentials and operation parallelism.

* This is a pure Erlang variant of the original Enera Library.

Named after Saci which is a character in Brazilian folklore, a one-legged black or  youngster, who smokes a pipe and wears a magical red cap that enables him to disappear and reappear wherever he wishes (usually in the middle of a dust devil). [Saci](https://en.wikipedia.org/wiki/Saci_(Brazilian_folklore))
The new name is a homage to its predecessor.

```
              `.                        
           :soydho+oos+-                
               +mdddddddh-              
                `:dmmmdddh`             
                  +mddhhhd-             
     `     `:ss/-` -ydddmm-.-.          
     `/+++oo+//yddsoodd+o:./hh.         
    `/os:.      .mdydd:   so+/.         
                ymmhddh- :d`            
               :dddh:-yhsys             
               oddh:   .+o`             
               sdhh+-                   
               `/ohdhy/                 
                   ./hd+                
                    +dh-                
                    ys`                 
                  .yd`                  
                  :hdo                  
               ```..yds:    `           
                ```.-:-.                             
```
                              

## Requirements

* [Erlang/OTP](http://www.erlang.org) R18 or R19
    * R20 has backward incompatible API changes
* [rebar3](https://github.com/erlang/rebar3/) 3.0.0 or higher

## Building and Testing

```
$ rebar3 compile
$ export GOOGLE_APPLICATION_CREDENTIALS=~/.your_gcp_credentials.json
$ rebar3 ct
```

Note that the test suite expects to find an environment variable named `GOOGLE_APPLICATION_CREDENTIALS` which specifies the path to your Google Cloud Platform service credentials. This JSON formatted file is created via the Cloud Console, as described in the setup section below. This file contains your private key, so be sure to store this file with permissions that prevent exposure to third parties.

## Example Usage

To include `saci` as a dependency in your release, add it to the list of dependencies in your `rebar.config` file, like so:

```
{deps, [
    {saci, {git, "https://github.com/nlfiedler/saci", {tag, "0.3.0"}}}
]}.
```

To have the `saci` application started automatically, be sure to include `saci` in the `applications` list of your application configuration before building a release. You may also want to add `jiffy` to the list of `included_applications`, and `hackney` to the list of `applications`.

Below is a simple example in which a bucket is created and a file is uploaded to that bucket. This also demonstrates loading the credentials from a file and computing an MD5 checksum of a file to be uploaded to a bucket.

```erlang
1> rr("include/saci.hrl").
[access_token,bucket,credentials,object]
2> application:ensure_all_started(saci).
{ok,[idna,mimerl,certifi,ssl_verify_fun,metrics,hackney,saci]}
3> Credentials = os:getenv("GOOGLE_APPLICATION_CREDENTIALS").
"/Users/nfiedler/.gcloud/testing.json"
4> {ok, Creds} = saci:load_credentials(Credentials).
{ok,#credentials{type = <<"service_account">>,
                 project_id = <<"a-project">>,
                 private_key_id = <<"a-private-key-id">>,
                 private_key = <<"a-private-key">>,
                 client_email = <<"an-email-address">>,
                 client_id = <<"a-client-id">>}}
5> saci:insert_bucket(#bucket{
    name = <<"0136d00f-a942-11e6-8f9a-3c07547e18a6-saci-1234">>,
    location = <<"US-WEST1">>,
    storageClass = <<"STANDARD">>}, Creds).
{ok,#bucket{id = <<"a-bucket-id">>,
            projectNumber = <<"a-project-number">>,
            name = <<"0136d00f-a942-11e6-8f9a-3c07547e18a6-saci-1234">>,
            timeCreated = <<"2016-11-18T22:25:54.239Z">>,
            updated = <<"2016-11-18T22:25:54.239Z">>,
            location = <<"US-WEST1">>,
            storageClass = <<"STANDARD">>}}
6> {ok, Md5} = saci:compute_md5("test/saci_SUITE_data/IMG_5745.JPG").
{ok,<<"kq56YDAH2p4mzAqrQw84kQ==">>}
7> saci:upload_file("test/saci_SUITE_data/IMG_5745.JPG", #object{
    name = <<"my_image">>,
    bucket = <<"0136d00f-a942-11e6-8f9a-3c07547e18a6-saci-1234">>,
    contentType = <<"image/jpeg">>,
    md5Hash = Md5,
    size = 107302}, Creds).
{ok,#object{id = <<"a-bucket-id/name/an-object-id">>,
            name = <<"my_image">>,
            bucket = <<"0136d00f-a942-11e6-8f9a-3c07547e18a6-saci-1234">>,
            contentType = <<"image/jpeg">>,
            timeCreated = <<"2016-11-18T22:28:01.232Z">>,
            updated = <<"2016-11-18T22:28:01.232Z">>,
            storageClass = <<"STANDARD">>,
            size = <<"107302">>,
            md5Hash = <<"kq56YDAH2p4mzAqrQw84kQ==">>}}
```

## Google Cloud Setup

1. Visit https://console.cloud.google.com/ and log in with your account.
1. Select an existing project or create a new one.
1. Note the *Project ID* as that will be necessary when connecting via saci.
1. Using the menu, select **API Manager**, then **Credentials**.
1. On the **Credentials** page, select the **Create credentials** drop-down, then select **Service account key**.
1. From the **Service** account drop-down, select an existing service account or create a new one.
1. For **Key** type, select the **JSON** key option, then select **Create**. The file automatically downloads to your computer.
1. Put the `*.json` file you just downloaded in a directory of your choosing. This directory must be private, but accessible to your application.

## Security

The credentials file must be readable by the application, but should not be readable by casual users. Likewise, it is possible, if the server process crashes, that the private keys will end up in the log file (they are an argument to the API, after all). As such, the log files should be protected from unintended exposure to third parties.

## Docker

[Docker](https://www.docker.com) can be used to build and test the code without affecting your development environment, which may have a different version of Erlang/OTP installed. The use of `docker-compose`, as shown in the example below, is optional, but it makes the process very simple. Note that `GOOGLE_APPLICATION_CREDENTIALS` should have a path reachable within the container, such as `/src/creds.json`

```shell
$ cd docker
$ docker-compose build
$ docker-compose run saci
$ rebar3 clean
$ rebar3 compile
$ rebar3 ct
```

## License

[BSD 3-Clause](https://opensource.org/licenses/BSD-3-Clause), see the `LICENSE` file.
