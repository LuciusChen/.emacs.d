#!/bin/bash

# 检查是否安装了必要命令
command -v gpg >/dev/null 2>&1 || { echo >&2 "需要安装 gpg。"; exit 1; }

# 配置
BACKUP_DIR="$HOME/Library/CloudStorage/Dropbox/gpg"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
BACKUP_KEYS="$BACKUP_DIR/gpg_keys_$TIMESTAMP.tar.gz.gpg"
BACKUP_GNUPG="$BACKUP_DIR/gnupg_folder_$TIMESTAMP.tar.gz.gpg"

# 创建备份目录
mkdir -p "$BACKUP_DIR"

# 函数：读取密码并确认
read_password() {
    read -s -p "请输入加密备份的密码: " BACKUP_PASS
    echo
    read -s -p "请再次输入密码: " BACKUP_PASS_CONFIRM
    echo
    if [[ "$BACKUP_PASS" != "$BACKUP_PASS_CONFIRM" ]]; then
        echo "错误：两次输入的密码不一致！"
        exit 1
    fi
}

# 函数：备份 GPG 密钥
backup_keys() {
    echo "正在备份 GPG 密钥..."
    read_password

    # 导出公钥和私钥
    gpg --export --armor > "$BACKUP_DIR/gpg_public_keys_$TIMESTAMP.asc"
    gpg --export-secret-keys --armor > "$BACKUP_DIR/gpg_private_keys_$TIMESTAMP.asc"

    # 打包并加密
    tar -czvf - -C "$BACKUP_DIR" "gpg_public_keys_$TIMESTAMP.asc" "gpg_private_keys_$TIMESTAMP.asc" | \
        gpg --symmetric --cipher-algo AES256 --batch --passphrase "$BACKUP_PASS" -o "$BACKUP_KEYS"

    # 清理临时文件
    rm "$BACKUP_DIR/gpg_public_keys_$TIMESTAMP.asc" "$BACKUP_DIR/gpg_private_keys_$TIMESTAMP.asc"

    echo "GPG 密钥已备份到: $BACKUP_KEYS"
}

# 函数：备份 ~/.gnupg 文件夹
backup_gnupg() {
    echo "正在备份 ~/.gnupg 文件夹..."
    read_password

    # 打包并加密
    tar -czvf - -C "$HOME" .gnupg | \
        gpg --symmetric --cipher-algo AES256 --batch --passphrase "$BACKUP_PASS" -o "$BACKUP_GNUPG"

    echo "~/.gnupg 文件夹已备份到: $BACKUP_GNUPG"
}

# 函数：导入备份的 GPG 密钥
import_keys() {
    echo "正在导入 GPG 密钥..."
    read -p "请输入备份文件路径: " BACKUP_FILE
    read -s -p "请输入备份文件的密码: " BACKUP_PASS
    echo

    if [[ ! -f "$BACKUP_FILE" ]]; then
        echo "错误：备份文件不存在！"
        exit 1
    fi

    # 解压缩时自动找到密钥文件
    BACKUP_TMP_DIR=$(mktemp -d)
    trap "rm -rf $BACKUP_TMP_DIR" EXIT

    # 解密并导入
    gpg --decrypt --batch --passphrase "$BACKUP_PASS" "$BACKUP_FILE" | tar -xzvf - -C "$BACKUP_TMP_DIR"
    gpg --import "$BACKUP_TMP_DIR"/*.asc

    echo "GPG 密钥导入完成！"
}

# 主菜单
echo "请选择操作："
echo "1. 备份 GPG 密钥"
echo "2. 备份 ~/.gnupg 文件夹"
echo "3. 导入备份的 GPG 密钥"
read -p "输入选项 (1/2/3): " OPTION

case $OPTION in
    1)
        backup_keys
        ;;
    2)
        backup_gnupg
        ;;
    3)
        import_keys
        ;;
    *)
        echo "错误：无效选项！"
        exit 1
        ;;
esac
