terraform init -backend-config=backend.tfvars

terraform get

terraform apply -auto-approve -var-file=backend.tfvars
