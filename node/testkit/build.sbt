publishTo      := sonatypePublishToBundle.value
publish / skip := false
homepage       := Some(url("https://waves.tech/"))
developers := List(
  Developer("ismagin", "Ilya Smagin", "ilya.smagin@gmail.com", url("https://github.com/ismagin")),
  Developer("asayadyan", "Artyom Sayadyan", "xrtm000@gmail.com", url("https://github.com/xrtm000")),
  Developer("mpotanin", "Mike Potanin", "mpotanin@wavesplatform.com", url("https://github.com/potan")),
  Developer("irakitnykh", "Ivan Rakitnykh", "mrkr.reg@gmail.com", url("https://github.com/mrkraft")),
  Developer("akiselev", "Alexey Kiselev", "alexey.kiselev@gmail.com>", url("https://github.com/alexeykiselev")),
  Developer("phearnot", "Sergey Nazarov", "snazarov@web3tech.ru", url("https://github.com/phearnot")),
  Developer("tolsi", "Sergey Tolmachev", "tolsi.ru@gmail.com", url("https://github.com/tolsi")),
  Developer("vsuharnikov", "Vyatcheslav Suharnikov", "arz.freezy@gmail.com", url("https://github.com/vsuharnikov")),
  Developer("ivan-mashonskiy", "Ivan Mashonskii", "ivan.mashonsky@gmail.com", url("https://github.com/ivan-mashonskiy"))
)

Compile / packageDoc / publishArtifact := true
Test / packageDoc / publishArtifact    := false
