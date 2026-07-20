# ASP Toolkit

<p align="center">
  <img src="www/hanyang_logo.png" alt="Hanyang University Medical Center" height="72">
</p>

ASP Toolkit은 병원 내 환자, 진단 및 항생제 사용 데이터를 한 화면에서 살펴보기 위한 웹 기반 분석 도구입니다. Excel 파일을 업로드하면 환자 현황과 진단 분포를 시각화하고, 항생제 사용량을 DOT와 DDD 지표로 분석할 수 있습니다.

> 이 프로그램은 연구 및 항생제 적정사용 지원을 위한 분석 도구입니다. 임상적 판단이나 의학적 의사결정을 대신하지 않습니다.

## 가장 간단한 사용 방법: 웹에서 실행

아래 링크를 누르면 별도 설치 없이 웹 브라우저에서 프로그램을 사용할 수 있습니다.

**[ASP Toolkit 실행하기](https://kimsangrok92.github.io/hanyang-asp/)**

웹 버전은 프로그램을 다운로드하거나 R을 설치할 필요가 없습니다. 처음 화면이 표시될 때 브라우저에서 분석 환경을 준비하므로 잠시 시간이 걸릴 수 있습니다. Chrome 또는 Edge의 최신 버전 사용을 권장합니다.

기관 정책상 외부 웹페이지 사용이 제한되거나 인터넷 연결 없이 사용해야 한다면 아래의 `로컬 컴퓨터에서 실행하기` 안내를 따라 프로그램을 내려받아 실행하세요.

## 예제 데이터로 테스트하기

처음 사용하는 경우에는 실제 자료보다 합성 예제 데이터로 프로그램의 화면과 필터를 먼저 확인하는 것을 권장합니다.

**[ASP Toolkit 예제 Excel 다운로드](examples/ASP_Toolkit_Example.xlsx)**

예제 파일을 이용하는 방법은 다음과 같습니다.

1. 위 링크를 누릅니다.
2. GitHub에서 파일 화면이 열리면 오른쪽의 다운로드 아이콘 또는 `Download raw file`을 눌러 파일을 저장합니다.
3. [ASP Toolkit 웹 실행 화면](https://kimsangrok92.github.io/hanyang-asp/)을 엽니다.
4. 왼쪽 `DATA` 영역에서 `Browse...`를 누릅니다.
5. 내려받은 `ASP_Toolkit_Example.xlsx`를 선택합니다.
6. 화면 상단에 `PATIENTS`, `DIAGNOSIS`, `DOT / DDD` 메뉴가 모두 나타나는지 확인합니다.
7. 왼쪽 필터에서 기간이나 항목을 선택한 후 `CALCULATE`를 누릅니다.

예제 Excel의 모든 환자번호, 입원기록, 진단 및 항생제 투여정보는 프로그램 시연을 위해 생성한 가상 데이터입니다. 실제 환자나 실제 진료기록을 나타내지 않으며 임상적 의사결정에 사용할 수 없습니다.

| 시트 | 예제 데이터 |
| --- | ---: |
| `Sheet1` | 가상 진단기록 100행 |
| `Sheet2` | 가상 처방 원자료 100행 |
| `Sheet3` | 가상 입원기록 100행 |
| `Sheet4` | 가공된 가상 항생제 분석자료 100행 |
| `Sheet5` | 2025년 월별 재원일수 12행 |

## 주요 기능

| 대시보드 | 제공 내용 |
| --- | --- |
| Patients | 월별 입원 건수, 재원기간 분포, 성별 분포, 환자 데이터 표 |
| Diagnosis | 월별 진단 건수, 진료과별 분포, 상위 20개 진단명 |
| DOT / DDD | 전체·항생제 계열별·성분명별 월간 추이와 월별·연도별 요약표 |

DOT(Days of Therapy)는 환자가 항생제를 투여받은 일수를 기반으로 한 사용량 지표이며, DDD(Defined Daily Dose)는 WHO의 성인 1일 표준 유지용량을 기반으로 한 지표입니다. 이 프로그램의 DOT와 DDD 결과는 재원일수 자료를 이용해 1,000 patient-days 기준으로 계산됩니다.

## 사용 방법

1. [ASP Toolkit](https://kimsangrok92.github.io/hanyang-asp/)에 접속합니다.
2. 왼쪽 `DATA` 영역의 `Browse...` 버튼을 누릅니다.
3. 분석할 `.xlsx` 파일을 선택합니다. 최대 업로드 크기는 100 MB입니다.
4. 파일에서 인식된 데이터에 따라 `PATIENTS`, `DIAGNOSIS`, `DOT / DDD` 메뉴가 나타납니다.
5. 왼쪽 사이드바에서 기간과 분석 조건을 선택합니다.
6. `CALCULATE` 버튼을 눌러 표와 그래프에 선택 조건을 반영합니다.
7. 화면 상단의 메뉴 버튼을 누르면 해당 대시보드로 바로 이동합니다.

필터의 `ALL`을 선택하면 해당 항목 전체가 분석에 포함됩니다. 일부 시트만 포함된 Excel 파일도 사용할 수 있으며, 데이터가 존재하는 대시보드만 화면에 표시됩니다.

## 개인정보 및 데이터 보안

실제 임상 데이터를 사용할 때는 소속 기관의 개인정보 보호 및 정보보안 정책을 따라야 합니다.

- 이름, 주민등록번호 등 직접 식별정보를 제거한 비식별 데이터를 사용하세요.
- 공개 저장소에 실제 환자 데이터나 분석 결과 파일을 커밋하지 마세요.
- 화면 공유나 결과 내보내기 전에도 환자 식별 가능성을 확인하세요.
- 외부 공개용 페이지에서는 테스트 데이터 또는 공개가 승인된 데이터만 사용하세요.

이 저장소에는 실제 환자 데이터가 포함되지 않습니다. `examples` 폴더의 Excel 파일은 프로그램 테스트를 위해 생성한 합성 데이터만 포함합니다.

## 로컬 컴퓨터에서 실행하기

로컬 실행은 프로그램과 분석 환경을 자신의 컴퓨터에 설치하여 사용하는 방법입니다. 아래 설명은 Windows 사용자를 기준으로 작성했으며, Docker는 필요하지 않습니다.

### 1. GitHub에서 프로그램 다운로드

GitHub를 처음 사용하는 경우에는 ZIP 다운로드 방식이 가장 간단합니다.

1. [hanyang-asp GitHub 저장소](https://github.com/KimSangrok92/hanyang-asp)에 접속합니다.
2. 파일 목록 위쪽에 있는 초록색 `Code` 버튼을 누릅니다.
3. 열린 메뉴에서 `Download ZIP`을 누릅니다.
4. 다운로드 폴더에 저장된 `hanyang-asp-main.zip` 파일을 찾습니다.
5. ZIP 파일을 마우스 오른쪽 버튼으로 누르고 `압축 풀기` 또는 `모두 압축 풀기`를 선택합니다.
6. 압축을 푼 `hanyang-asp-main` 폴더를 문서 폴더 등 찾기 쉬운 위치로 옮깁니다.

바로 다운로드하려면 **[프로그램 ZIP 파일 받기](https://github.com/KimSangrok92/hanyang-asp/archive/refs/heads/main.zip)**를 누를 수도 있습니다.

압축을 푼 폴더 안에 아래 파일과 폴더가 있는지 확인하세요.

```text
hanyang-asp-main/
├── app.R
└── www/
    ├── hanyang_logo.png
    └── fonts/
```

`app.R` 파일만 따로 옮기면 로고와 한글 글꼴이 정상적으로 표시되지 않을 수 있습니다. 반드시 `www` 폴더와 함께 보관하세요.

### 2. R 설치

ASP Toolkit은 R로 작성되어 있으므로 로컬 실행을 위해 R이 반드시 필요합니다.

1. [R for Windows 공식 다운로드 페이지](https://cran.r-project.org/bin/windows/base/)에 접속합니다.
2. 페이지 상단의 `Download R for Windows` 설치 파일을 받습니다.
3. 다운로드한 설치 파일을 실행합니다.
4. 특별한 설정이 필요하지 않으면 기본값을 유지한 채 설치를 완료합니다.

이미 R이 설치되어 있다면 이 단계는 건너뛰어도 됩니다.

### 3. RStudio Desktop 설치

RStudio는 R을 편리하게 사용할 수 있게 해주는 프로그램입니다. 필수는 아니지만 처음 사용하는 경우 설치를 권장합니다.

1. R을 먼저 설치합니다.
2. [RStudio Desktop 공식 다운로드 페이지](https://posit.co/download/rstudio-desktop/)에 접속합니다.
3. Windows용 RStudio Desktop 설치 파일을 받습니다.
4. 설치 파일을 실행하고 기본값으로 설치를 완료합니다.
5. 설치가 끝나면 RStudio를 실행합니다.

### 4. 프로그램 폴더 열기

1. RStudio를 실행합니다.
2. 상단 메뉴에서 `File` → `Open File...`을 선택합니다.
3. 압축을 풀어둔 `hanyang-asp-main` 폴더의 `app.R`을 엽니다.
4. 상단 메뉴에서 `Session` → `Set Working Directory` → `To Source File Location`을 선택합니다.

RStudio의 `Console` 창에 다음 명령을 입력하면 현재 작업 폴더를 확인할 수 있습니다.

```r
getwd()
list.files()
```

`list.files()` 결과에 `app.R`과 `www`가 보이면 올바른 폴더가 선택된 것입니다.

### 5. 필요한 R 패키지 설치

다음 코드를 RStudio의 `Console` 창에 붙여 넣고 Enter를 누릅니다. 이 작업은 처음 실행할 때 한 번만 하면 됩니다.

```r
required_packages <- c(
  "shiny", "shinydashboard", "shinydashboardPlus", "shinyBS",
  "rintrojs", "readxl", "dplyr", "ggplot2", "DT", "lubridate",
  "scales", "stringr", "tidyr", "bslib", "showtext", "sysfonts"
)

new_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}
```

패키지는 인터넷에서 내려받기 때문에 컴퓨터 환경에 따라 몇 분 정도 걸릴 수 있습니다. 설치 중에는 RStudio를 종료하지 마세요. 오류 없이 `>` 기호가 다시 나타나면 설치가 끝난 것입니다.

### 6. 프로그램 실행

패키지 설치가 끝나면 RStudio의 `Console` 창에서 다음 명령을 실행합니다.

```r
shiny::runApp(".", launch.browser = TRUE)
```

잠시 후 웹 브라우저가 자동으로 열립니다. 브라우저가 열리지 않으면 RStudio Console에 표시된 `http://127.0.0.1:포트번호` 주소를 복사하여 Chrome 또는 Edge 주소창에 입력하세요.

화면이 열리면 왼쪽 `DATA` 영역의 `Browse...` 버튼으로 분석할 Excel 파일을 선택합니다. Excel 파일은 프로그램 폴더 안에 둘 필요가 없으며, 원본 파일의 위치에서 바로 선택할 수 있습니다.

### 7. 프로그램 종료

브라우저 창만 닫아도 R 프로그램은 계속 실행될 수 있습니다.

- RStudio에서 실행했다면 Console 위쪽의 빨간색 `Stop` 버튼을 누릅니다.
- 터미널에서 실행했다면 터미널 창에서 `Ctrl + C`를 누릅니다.

종료 후에는 로컬 접속 주소도 더 이상 열리지 않습니다.

### 8. 다음에 다시 실행할 때

패키지는 매번 다시 설치할 필요가 없습니다. 컴퓨터를 재부팅했거나 프로그램을 종료한 뒤에는 다음 과정만 반복하면 됩니다.

1. RStudio를 실행합니다.
2. `hanyang-asp-main/app.R`을 엽니다.
3. `Session` → `Set Working Directory` → `To Source File Location`을 선택합니다.
4. Console에서 아래 명령을 실행합니다.

```r
shiny::runApp(".", launch.browser = TRUE)
```

### PowerShell에서 실행하는 방법

RStudio 대신 PowerShell을 사용할 수도 있습니다. 먼저 PowerShell에서 프로그램 폴더로 이동합니다. 아래 경로는 예시이므로 실제 압축 해제 위치에 맞게 변경해야 합니다.

```powershell
cd "C:\Users\사용자이름\Downloads\hanyang-asp-main"
Rscript -e "shiny::runApp('.', host='127.0.0.1', port=3838, launch.browser=TRUE)"
```

정상 실행되면 `http://127.0.0.1:3838` 또는 `http://localhost:3838`로 접속할 수 있습니다. 종료할 때는 PowerShell에서 `Ctrl + C`를 누릅니다.

`Rscript`를 찾을 수 없다는 메시지가 나오면 R 실행 경로가 Windows 환경 변수에 등록되지 않은 상태입니다. 이 경우에는 위의 RStudio 실행 방법을 사용하면 됩니다.

### Git을 사용할 수 있는 사용자를 위한 방법

Git이 설치되어 있다면 ZIP 파일 대신 저장소를 복제할 수 있습니다.

```powershell
git clone https://github.com/KimSangrok92/hanyang-asp.git
cd hanyang-asp
Rscript -e "shiny::runApp('.', launch.browser=TRUE)"
```

나중에 최신 프로그램을 받을 때는 해당 폴더에서 다음 명령을 실행합니다.

```powershell
git pull origin main
```

ZIP 방식으로 내려받은 사용자는 `git pull`을 사용할 수 없습니다. 최신 버전이 필요할 때 GitHub에서 ZIP 파일을 다시 내려받아 새 폴더에 압축을 풀어야 합니다.

### 로컬 실행 문제 해결

#### `app.R`을 찾을 수 없다는 오류

현재 작업 폴더가 잘못된 경우입니다. RStudio에서 `app.R`을 연 다음 `Session` → `Set Working Directory` → `To Source File Location`을 다시 선택하세요.

#### `there is no package called ...` 오류

오류 메시지에 표시된 패키지가 설치되지 않은 경우입니다. 위의 `필요한 R 패키지 설치` 코드를 다시 실행한 뒤 프로그램을 시작하세요.

#### 로컬 주소가 열리지 않는 경우

RStudio Console에 프로그램이 실행 중인지 확인합니다. 프로그램을 종료한 상태라면 `shiny::runApp(".", launch.browser = TRUE)`를 다시 실행해야 합니다.

#### 포트를 사용할 수 없다는 오류

다른 프로그램이 같은 포트를 사용 중일 수 있습니다. 다른 포트로 실행하세요.

```r
shiny::runApp(".", port = 3839, launch.browser = TRUE)
```

이 경우 접속 주소는 `http://127.0.0.1:3839`가 됩니다.

#### Excel 파일을 업로드해도 화면이 나타나지 않는 경우

- 파일 확장자가 `.xlsx`인지 확인합니다.
- 파일 크기가 100 MB 이하인지 확인합니다.
- 필요한 시트 이름과 주요 컬럼이 예제 파일과 일치하는지 확인합니다.
- 암호가 설정된 Excel 파일은 암호를 해제한 사본으로 다시 시도합니다.
- DOT / DDD 화면에는 `Sheet4`와 `Sheet5`가 모두 필요합니다.

## 저장소 구성

```text
hanyang-asp/
├── app.R                         # 현재 ASP Toolkit 애플리케이션
├── app_clean.R                   # 이전 정리 버전 보관본
├── examples/
│   └── ASP_Toolkit_Example.xlsx  # 가상 데이터 예제 파일
├── scripts/
│   └── create_example_workbook.py # 예제 파일 재생성 스크립트
├── www/
│   ├── hanyang_logo.png          # 화면 및 README 로고
│   └── fonts/NotoSansKR-Regular.otf
└── .github/workflows/
    └── deploy-shinylive.yml      # GitHub Pages 자동 배포
```

`main` 브랜치에 변경사항이 push되면 GitHub Actions가 Shinylive 앱을 빌드하여 GitHub Pages에 자동으로 배포합니다.

## 기술 구성

- R, Shiny, Shinydashboard
- ggplot2, DT, dplyr, tidyr
- readxl
- Shinylive 및 GitHub Pages
