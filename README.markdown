# Concepts

Retex has three types of objects: formulas, challenges and responses. Formulas are created by uploading formula images. A challenge is a collection of formulas generated for a specific use eg a retex captcha. Challenges expire after a short time (default 5 minutes). A response is the latex code submitted by a user in response to a challenge. All three objects are identified by 32 character alphanumeric ids.

# API

The retex server exports a RESTful json API. This API is intended to be used behind the applications server and not exposed to the public. See 'src/types.hrl' for information about the meaning of various fields.

Any object can be looked up by id. Not all objects currently provide useful information.

    jamie@alien:~/retex$ curl -X GET http://localhost:8000/formula/form05403h5x0j46614j5l256m61180c671h3m146s610c24382e5j2r1h5r
    {"id":"form05403h5x0j46614j5l256m61180c671h3m146s610c24382e5j2r1h5r","url":"image/form05403h5x0j46614j5l256m61180c671h3m146s610c24382e5j2r1h5r.png","latex":null}

    jamie@alien:~/retex$ curl -X GET http://localhost:8000/challenge/chal59376l0j5a3v0n4i54330k4b3y4e6q17134x20271y075l1h1l3p0d6x
    {"id":"chal59376l0j5a3v0n4i54330k4b3y4e6q17134x20271y075l1h1l3p0d6x","formulas":["form05403h5x0j46614j5l256m61180c671h3m146s610c24382e5j2r1h5r"]}j

    jamie@alien:~/retex$ curl -X GET http://localhost:8000/response/resp4h214x6p46662v2m1r0q064u6y5e5e166o6d39704q4q3j08471o5y62
    {"id":"resp4h214x6p46662v2m1r0q064u6y5e5e166o6d39704q4q3j08471o5y62"}

An example of a latexpert style interaction.

    jamie@alien:~/retex$ curl -X POST -H 'Content-Type: application/json' -d '{"source":"www.example.com"}' http://localhost:8000/challenge
    {"id":"chal59376l0j5a3v0n4i54330k4b3y4e6q17134x20271y075l1h1l3p0d6x","formulas":["form05403h5x0j46614j5l256m61180c671h3m146s610c24382e5j2r1h5r"]}

    jamie@alien:~/retex$ curl -X POST -H 'Content-Type: application/json' -d '{"challenge":"chal59376l0j5a3v0n4i54330k4b3y4e6q17134x20271y075l1h1l3p0d6x", "user":"some_username@example.com", "latexs":["$ 1+1=2 $"]}' http://localhost:8000/response
    {"id":"resp4h214x6p46662v2m1r0q064u6y5e5e166o6d39704q4q3j08471o5y62"}
    
    jamie@alien:~/retex$ curl -X GET http://localhost:8000/formula/form05403h5x0j46614j5l256m61180c671h3m146s610c24382e5j2r1h5r/stats
    {"responses":{"$ 1+1=2 $":2}}

# Setup

Requires [jailkit](http://olivier.sessink.nl/jailkit/).

Setup on Ubuntu:

    sudo apt-get install git-core erlang python python-plastex
    git clone git://github.com/jamii/retex.git
    cd retex
    sudo ./scripts/jail_init
    make

# Admin

Currently retex must be run as root in order to create the latex interpreter sandbox.

    sudo ./start 

    ./control upload /some/dir/full/of/images

    ./control backup /offsite/my_backup

    ./control restore /offsite/my_backup

    ./control stop