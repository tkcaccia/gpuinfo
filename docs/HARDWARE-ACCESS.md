# Hardware access and validation playbook

This document records where `gpuinfo` can be tested, which accounts are
required, and how to preserve reproducible evidence. Service availability and
prices change, so check the linked provider page before launching hardware.

## Access ledger

| Target | Service | Account status | Validation status |
|---|---|---|---|
| Apple Silicon / Metal | Local Apple M3 | Available | Validated |
| Linux, Windows, Intel macOS, Apple Silicon macOS | GitHub Actions | Active as `tkcaccia` | Validated |
| ARM64 Linux CPU | GitHub Actions `ubuntu-24.04-arm` | Active as `tkcaccia` | Validated |
| NVIDIA CUDA on Linux | Hugging Face Jobs `t4-small` | Active as `tkcaccia`; billing enabled | Tesla T4 validated |
| AMD ROCm on Linux | AMD Developer Cloud | Account active; credit request submitted 2026-09-05 and awaiting review | Not yet validated |
| Intel OpenCL/Level Zero on Linux | Intel Cloud Services | Registration prepared 2026-09-05; password and email verification pending | Not yet validated |
| NVIDIA CUDA/OpenCL on Windows | Windows GPU VM or self-hosted runner | Provider pending | Not yet validated |

Never put passwords, API tokens, cloud credentials, SSH private keys, or
payment details in this repository. Store credentials in the provider account,
the operating-system keychain, or encrypted GitHub Actions secrets. Only public
job IDs, hardware descriptions, and non-secret output belong in validation
fixtures.

## Common validation procedure

Use a clean public commit so the exact source can be recovered later. On the
target machine:

```sh
git clone https://github.com/tkcaccia/gpuinfo.git
cd gpuinfo
git rev-parse HEAD
R CMD INSTALL .
Rscript tools/validate-hardware.R BACKEND | tee validation-output.txt
```

Replace `BACKEND` with `cuda`, `metal`, `rocm`, or `opencl`. A successful run
must end with `VALIDATION PASSED`. Also record:

- commit SHA, date, OS, architecture, provider, and public job ID or URL;
- physical GPU model/count, driver and runtime versions, and total memory;
- the complete `gpu_sitrep()` output;
- whether the VM or job was destroyed or completed.

Add a concise raw-output fixture under `tests/fixtures/<vendor>/`, add one row
to `inst/extdata/validation.csv`, and update `inst/VALIDATION.md`. Do not mark a
platform validated from parser mocks or CPU-only CI.

## GitHub Actions: operating systems and CPU architectures

Account: <https://github.com/signup>

Repository Actions: <https://github.com/tkcaccia/gpuinfo/actions>

The normal workflow covers Linux x86-64, Windows x86-64, Intel macOS, Apple
Silicon macOS, and Linux ARM64. GitHub documents hosted runner labels including
`ubuntu-24.04-arm`, `windows-11-arm`, `macos-15-intel`, and Apple Silicon macOS:
<https://docs.github.com/en/actions/reference/runners/github-hosted-runners>.

The first ARM64 run passed on 2026-09-05:
<https://github.com/tkcaccia/gpuinfo/actions/runs/33967507957>.

Real GPUs can be attached as ephemeral self-hosted runners. Assign labels such
as `nvidia,cuda`, `amd,rocm`, or `intel,opencl`, then manually dispatch
`.github/workflows/hardware-validation.yaml`. GitHub's runner setup and label
rules are documented at:
<https://docs.github.com/en/actions/how-tos/manage-runners/self-hosted-runners/use-in-a-workflow>.

Treat self-hosted runners as sensitive infrastructure: use an ephemeral VM,
restrict repository access, do not expose long-lived secrets to pull requests,
and destroy the VM after collecting the artifact.

## NVIDIA Linux: Hugging Face Jobs

Account and billing: <https://huggingface.co/settings/billing>

Jobs dashboard: <https://huggingface.co/jobs>

Official Jobs guide:
<https://huggingface.co/docs/huggingface_hub/guides/jobs>

For this lightweight probe, use the least expensive available NVIDIA flavor,
currently `t4-small`, and a short timeout. The public repository needs no
`HF_TOKEN`; a token is only required for private repositories or Hub uploads.

Equivalent CLI recipe:

```sh
hf jobs run \
  --flavor t4-small \
  --timeout 20m \
  nvidia/cuda:12.6.3-devel-ubuntu22.04 \
  bash -lc 'set -euo pipefail; apt-get update; DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends r-base r-base-dev git ca-certificates build-essential; git clone --depth 1 https://github.com/tkcaccia/gpuinfo.git /tmp/gpuinfo; cd /tmp/gpuinfo; R CMD INSTALL .; Rscript tools/validate-hardware.R cuda'
```

Useful commands are `hf jobs ps`, `hf jobs logs JOB_ID`, and
`hf jobs cancel JOB_ID`. Always set a timeout. Jobs are ephemeral, so copy the
logs before relying on them as evidence.

Completed validations:

- Initial T4 run: <https://huggingface.co/jobs/tkcaccia/6a9c0df1e686246ca69a3ada>
- Deduplication regression run: <https://huggingface.co/jobs/tkcaccia/6a9c0fdbe686246ca69a3b08>

## AMD Linux: AMD Developer Cloud

Start here:
<https://www.amd.com/en/developer/resources/cloud-access/amd-developer-cloud.html>

Credit instructions:
<https://www.amd.com/en/developer/resources/technical-articles/2026/how-to-claim-amd-cloud-credits.html>

Cloud dashboard: <https://devcloud.amd.com/>

Credit request form: <https://try.digitalocean.com/amd-cloud-free-credit/>

### Account status (2026-09-05)

- The AMD AI Developer Program account is active and shows a 100-credit member
  benefit.
- The AMD Developer Cloud sign-in method was changed from GitHub to AMD SSO, so
  use the **Sign in with AMD** option for this cloud account.
- The cloud billing page still showed **$0 credits applied**. The 100-credit
  member benefit is not spendable in the cloud until the separate credit
  request is approved and applied.
- The DigitalOcean-hosted AMD GPU credit form was submitted successfully on
  2026-09-05. Its confirmation said the request was assigned to the team for
  review and that follow-up would arrive by email.
- No payment method, SSH key, or GPU Droplet has been added or created yet.

Wait for the approval email, then sign in at the cloud dashboard and verify the
credit appears on the billing page before creating hardware. At the time of the
request, the create page offered one on-demand MI300X (192 GB VRAM, 20 vCPU,
240 GB RAM) with an Ubuntu 24.04/ROCm image at $1.99 per GPU-hour. Recheck the
current price and image before creation because these details can change.

1. Create an AMD account and join the AMD AI Developer Program.
2. Open **Member Perks**, request cloud credits, and complete the verification
   form. Approval can take several business days.
3. Link or create the third-party AMD Developer Cloud account and confirm the
   credit balance is visible.
4. Add an SSH public key. Never commit the private key.
5. Create the smallest single-MI300X instance and select the official ROCm
   software image.
6. Install R, its development headers, Git, and a C compiler if the image does
   not already contain them.
7. Run the common procedure with backend `rocm`.
8. Download `validation-output.txt`, then **destroy the instance**.

AMD warns that a powered-off VM can remain billable because its resources are
reserved. Destroy it after validation; merely shutting it down is insufficient.

## Intel Linux: Intel Cloud Services

Portal: <https://cloud.intel.com/>

Documentation: <https://cloud.intel.com/docs/index.html>

Intel GPU access overview:
<https://www.intel.com/content/www/us/en/developer/platform/data-center-gpu-max.html>

The former `console.cloud.intel.com` address no longer resolves. Use
`cloud.intel.com`. On 2026-09-05, registration was prepared with the account
holder's name, United Kingdom English, and South Africa; password creation and
email verification still require the account holder. The newsletter option was
left unchecked.

The portal describes this workflow: request hardware from the catalog, wait for
approval (normally up to three days), then access the approved instance in the
browser or by SSH. Intel's GPU Max page advertises free evaluation access for
up to 120 days, with possible extensions. Verify the exact entitlement shown in
the account before requesting an instance.

1. Create an Intel Cloud Services account and activate the available trial or
   evaluation entitlement.
2. Request a system with an Intel Data Center GPU Max or Flex device.
3. Select a oneAPI image containing the Intel GPU driver and OpenCL/Level Zero
   runtime.
4. Confirm `clinfo` enumerates a GPU, install R/build tools, and run the common
   procedure with backend `opencl`.
5. Save the output and delete the instance.

The current package tests OpenCL. A future Level Zero backend should be treated
as a separate capability rather than assuming that oneAPI implies OpenCL.

## NVIDIA Windows

The most reproducible choices are:

1. a temporary Windows NVIDIA GPU VM, such as an AWS EC2 G4dn instance; or
2. a trusted Windows workstation registered as an ephemeral GitHub
   self-hosted runner.

AWS account: <https://signin.aws.amazon.com/signup>

AWS documents G4dn instances with NVIDIA T4 GPUs and Windows NVIDIA-driver
installation in the EC2 documentation:
<https://docs.aws.amazon.com/ec2/latest/instancetypes/ec2-types.pdf>.

Install the NVIDIA driver, R, Rtools, and Git. In PowerShell:

```powershell
git clone https://github.com/tkcaccia/gpuinfo.git
Set-Location gpuinfo
R CMD INSTALL .
Rscript tools/validate-hardware.R cuda | Tee-Object validation-output.txt
```

Confirm both the native `nvcuda.dll` probe and `nvidia-smi` results. Stop and
terminate the VM after saving the output; verify that attached disks and public
IP resources are also removed if they are not needed.

## Local Apple Metal

No external account is required. From the repository on an Apple Silicon Mac:

```sh
R CMD INSTALL .
Rscript tools/validate-hardware.R metal | tee validation-output.txt
```

Test Intel macOS separately through GitHub Actions because an Apple Silicon
machine running translation is not equivalent to native Intel hardware.

## Cost and cleanup checklist

Before launching:

- verify the provider, region, GPU model, hourly price, and credit balance;
- choose one GPU and the smallest suitable machine;
- use an immutable public commit and set a 15–20 minute job timeout when the
  provider supports it;
- avoid persistent volumes unless they are required.

After validation:

- preserve logs and the exact commit SHA;
- cancel failed jobs immediately;
- destroy VMs rather than only stopping them;
- check the provider dashboard for running resources and unexpected storage;
- record the validation in the repository without recording credentials.
