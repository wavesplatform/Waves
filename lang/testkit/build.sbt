publishTo      := sonatypePublishToBundle.value
publish / skip := false
homepage       := Some(url("https://docs.waves.tech/en/ride/"))
developers := List(
  Developer("ismagin", "Ilya Smagin", "ilya.smagin@gmail.com", url("https://github.com/ismagin")),
  Developer("asayadyan", "Artyom Sayadyan", "xrtm000@gmail.com", url("https://github.com/xrtm000")),
  Developer("mpotanin", "Mike Potanin", "mpotanin@wavesplatform.com", url("https://github.com/potan")),
  Developer("irakitnykh", "Ivan Rakitnykh", "mrkr.reg@gmail.com", url("https://github.com/mrkraft"))
)

Compile / packageDoc / publishArtifact := true
Test / packageDoc / publishArtifact    := false
