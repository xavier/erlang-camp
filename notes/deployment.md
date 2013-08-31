# Release and Deployment

## OTP Applications

* Basic building blocks of systems
* Active applications
* Library applications

## Releases

* Set of versioned applications
* Describes the order of the applications
* Describes the starting configuration
* Can be packaged up to run on a target platform


    systools:make_script("foo-0.1.0", [local, {path, ["./ebin"]}]).
    systools:make_tar("foo-0.1.0", [{path, ["./ebin"]}, {erts, code:root_dir()}]).