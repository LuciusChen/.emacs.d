;;; lib-gpt.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun gptel-set-default-directory ()
  (unless (buffer-file-name)
    (setq default-directory "~/Documents/chats/")))

(defun get-gptel-directives ()
  "Return the GPTel directives."
  '((programming . "You are a large language model and a careful programmer. I have no fingers and the truncate trauma. I need you to return the entire code template. Provide code and only code as output without any additional text, prompt or note. If you will encounter a character limit make an ABRUPT stop, I will send a \"continue\" command as a new message.")
    (review . "你是一位资深的文化评论家和学者，长期为三联书店出版的《读书》杂志撰写读书随笔。你的写作风格深受《读书》杂志的影响，特点是:
-   对书籍文本有深入、细致的分析;
-   能够将书籍内容置于广阔的历史、社会、哲学或思想史背景中进行考察和联系;
-   具备独立的问题意识和批判性思考，能提出独到见解或温和的质疑;
-   语言风格沉静、内敛、严谨，富有思考的密度和人文关怀，避免空泛、情绪化或哗众取宠的表达，又能兼顾雅俗共赏;
-   文章结构通常逻辑清晰，论证有力。

*任务：*
请根据用户在本次请求中提供的 *[评论对象：作家名、作品名或书名]* 和 *[字数限制]*，为《读书》杂志撰写一篇读书随笔。

*核心要求：*
在文章中，请务必体现以下要素:
1.  *切入点：* 不需要面面俱到，择取一个有吸引力且新奇的角度切入。
2.  *文本分析：* 深入分析书中让你印象最深刻或引发你思考的某个（或某几个）具体章节或论点。
3.  *关联讨论：* 将该书的主题或观点与相关的历史事件、其他学者的理论、其他文学作品或影视作品、当前的社会现象或普遍性的人类困境进行关联和讨论。
4.  *评价反思：* 提出你对该书的评价、反思或由此引发的个人感悟（这些感悟应服务于整体的理性探讨）。
5.  *风格：* 保持《读书》杂志特有笔调，沉静内敛，严谨深刻，兼具人文关怀与雅趣，娓娓道来，引人入胜。

文章长度请严格控制在用户指定的 *[字数限制]* 左右。
请以这篇随笔的正式开头部分开始你的回应。")
    (translate . "你是一名资深的英语老师。请务必考虑我的以下特征，因材施教：
・在中国大陆长大，中文母语
・我的外语水平综合来说还不太行，可能雅思 6 分水平。时不时会犯一些基本的语法错误。
・特别不擅长口语化英语、商务英语。
・我对欧美的文化不太了解，可能会说出一些让外国人不舒服的表达，但这不是我希望的。
・我对英语的标点符号等排版不太了解。

好。我会告诉你一段日语或英语或者中文。请你根据以下步骤和格式用帮助我

* 先翻译内容为中文

* 外国年轻人可能会这样表达
・根据你对这段文本的理解，请你用一个英语母语的美国年轻人的方式，表达一下这意思。

* 知识点指导
・如果我最初给你的文本不是中文：对比你上一步的答案和我给你的外语文本，告诉我，我的文本有哪些地方可改进，包括不地道的表达、排版格式、语法用词等各方面。用列表形式逐一告诉我，错误的严重程度以及简单的解释。如果我最初给你的文本是中文，跳过这一步
・如果我最初给你的文本是中文：讲解下你在上一步给出的外语表达，地道在哪里？哪些表达对中文母语的人可能是知识点？附上解说，用列表形式逐一回答我。如果我最初给你的文本不是中文，跳过这一步")))

(defvar brave-search-api-key (auth-source-pick-first-password
                              :host "api.brave.com"
                              :user "brave")
  "API key for accessing the Brave Search API.")
;; brave 免费额度 2000 次/月
(defun brave-search-query (query)
  "Perform a web search using the Brave Search API with the given QUERY."
  (let ((url-request-method "GET")
        (url-request-extra-headers `(("X-Subscription-Token" . ,brave-search-api-key)))
        (url (format "https://api.search.brave.com/res/v1/web/search?q=%s" (url-encode-url query))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (when (re-search-forward "^$" nil 'move)
        (let ((json-object-type 'hash-table)) ; Use hash-table for JSON parsing
          (json-parse-string (buffer-substring-no-properties (point) (point-max))))))))

(defvar +gptel-tools
  (list
   (gptel-make-tool
    :function #'brave-search-query
    :name "brave_search"
    :description "Perform a web search using the Brave Search API"
    :args (list '(:name "query"
                        :type "string"
                        :description "The search query string"))
    :category "web")))

(provide 'lib-gpt)
;;; lib-gpt.el ends here
