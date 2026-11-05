publish / skip := false
homepage       := Some(uri("https://waves.tech/"))
developers := List(
  Developer("ismagin", "Ilya Smagin", "ilya.smagin@gmail.com", uri("https://github.com/ismagin")),
  Developer("asayadyan", "Artyom Sayadyan", "xrtm000@gmail.com", uri("https://github.com/xrtm000")),
  Developer("mpotanin", "Mike Potanin", "mpotanin@wavesplatform.com", uri("https://github.com/potan")),
  Developer("irakitnykh", "Ivan Rakitnykh", "mrkr.reg@gmail.com", uri("https://github.com/mrkraft")),
  Developer("akiselev", "Alexey Kiselev", "alexey.kiselev@gmail.com>", uri("https://github.com/alexeykiselev")),
  Developer("phearnot", "Sergey Nazarov", "phearnot@renee.ru", uri("https://github.com/phearnot")),
  Developer("tolsi", "Sergey Tolmachev", "tolsi.ru@gmail.com", uri("https://github.com/tolsi")),
  Developer("vsuharnikov", "Vyatcheslav Suharnikov", "arz.freezy@gmail.com", uri("https://github.com/vsuharnikov")),
  Developer("ivan-mashonskiy", "Ivan Mashonskii", "ivan.mashonsky@gmail.com", uri("https://github.com/ivan-mashonskiy"))
)

Compile / packageDoc / publishArtifact := true
Test / packageDoc / publishArtifact    := false
