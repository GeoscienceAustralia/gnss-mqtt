# taken from: https://github.com/WesleyCharlesBlake/terraform-aws-eks

data "terraform_remote_state" "remote_state" {
  backend = "s3"

  config {
    bucket         = "${var.bucket}"
    dynamodb_table = "${var.dynamodb_table}"
    region         = "${var.region}"
    key            = "terraform.tfstate"
  }
}

terraform {
  backend "s3" {}
}


module "eks" {
  source                    = "./modules/eks"
  cluster-name              = "${var.cluster-name}"
  k8s-version               = "${var.k8s-version}"
  node-instance-type        = "${var.node-instance-type}"
  desired-capacity          = "${var.desired-capacity}"
  max-size                  = "${var.max-size}"
  min-size                  = "${var.min-size}"
  vpc-subnet-cidr           = "${var.vpc-subnet-cidr}"
  workstation-external-cidr = "${var.workstation-external-cidr}"
  region                    = "${var.region}"
}
