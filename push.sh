#!/bin/bash

# GitHub Push Script
# Automatically pushes changes to GitHub using stored token

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Configuration
REPO_DIR="/home/scott/rpg-seu-c"
TOKEN_FILE="/home/scott/github_token"
REPO_URL="github.com/SComps/rpg-seu-c.git"

# Change to repository directory
cd "$REPO_DIR" || exit 1

# Check if token file exists
if [ ! -f "$TOKEN_FILE" ]; then
    echo -e "${RED}Error: Token file not found at $TOKEN_FILE${NC}"
    exit 1
fi

# Read token
TOKEN=$(cat "$TOKEN_FILE")

# Check git status
echo -e "${YELLOW}Checking repository status...${NC}"
git status

# Ask for confirmation
echo ""
read -p "Do you want to add all changes and commit? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    # Add all changes
    echo -e "${YELLOW}Adding all changes...${NC}"
    git add -A
    
    # Ask for commit message
    echo ""
    echo "Enter commit message (or press Enter for default):"
    read -r COMMIT_MSG
    
    if [ -z "$COMMIT_MSG" ]; then
        COMMIT_MSG="Update: $(date '+%Y-%m-%d %H:%M:%S')"
    fi
    
    # Commit changes
    echo -e "${YELLOW}Committing changes...${NC}"
    git commit -m "$COMMIT_MSG"
    
    if [ $? -ne 0 ]; then
        echo -e "${RED}Commit failed or no changes to commit${NC}"
        exit 1
    fi
fi

# Push to GitHub
echo -e "${YELLOW}Pushing to GitHub...${NC}"
git push https://${TOKEN}@${REPO_URL} main

if [ $? -eq 0 ]; then
    echo -e "${GREEN}Successfully pushed to GitHub!${NC}"
    echo -e "${GREEN}Repository: https://${REPO_URL}${NC}"
else
    echo -e "${RED}Push failed!${NC}"
    exit 1
fi

# Made with Bob
