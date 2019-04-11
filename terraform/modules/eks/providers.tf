variable "region" {}

provider "aws" {
  region = "${var.region}"
}

data "aws_availability_zones" "available" {}
