	package controllers.tweet

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents, Request}
import models.Tweet
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.I18nSupport

case class TweetFormData(content: String)

@Singleton
class TweetController @Inject()(val controllerComponents: ControllerComponents) extends BaseController with I18nSupport {
  val tweets = scala.collection.mutable.ArrayBuffer((1L to 10L).map(i => Tweet(Some(i), s"test tweet${i.toString}")): _*)

  val form = Form(
    //html formのnameがcontentのものを150文字以下の必須文字列に設定する
    mapping(
      "content" -> nonEmptyText(maxLength = 140)
    )(TweetFormData.apply)(TweetFormData.unapply)
  )

  def register() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.tweet.store(form))
  }

  //コンパイルエラー回避用
  def store(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    //fold: データ受け取りの成功・失敗を分岐しつつ処理を行える
    //fold()(): LeftとRightの処理
    //bindFromRequest: implicitでrequestを受け取る
    //そのため、リクエスト情報からformで設定したマッピング情報を元に入力チェックと値変換を行える
    //そして、その処理の成否によってfoldで処理を分岐しているという動き
    form.bindFromRequest().fold(
      //処理失敗時の処理
      (formWithErrors: Form[TweetFormData]) => {
        BadRequest(views.html.tweet.store(formWithErrors))
      },
      //処理成功時の処理
      (tweetFormData: TweetFormData) => {
        //seqにTweetを追加
        tweets += Tweet(Some(tweets.size + 1L), tweetFormData.content)
        //登録完了: 一覧画面へリダイレクト
        Redirect(routes.TweetController.list())
      }
    )
  }

  def list() = Action {implicit request: Request[AnyContent] =>
    Ok(views.html.tweet.list(tweets.toSeq))
  }

  def show(id: Long) = Action { implicit request: Request[AnyContent] =>
    tweets.find(_.id.exists(_ == id)) match {
      case Some(tweet) => Ok(views.html.tweet.show(tweet))
      case None        => NotFound(views.html.error.page404())
    }
  }

  /*編集画面を開く*/
  def edit(id: Long) = Action { implicit request: Request[AnyContent] =>
    tweets.find(_.id.exists(_ == id)) match {
      case Some(tweet) =>
        Ok(views.html.tweet.edit(
          //データを識別するために
          id,
          //fill: formに値を入れる
          form.fill(TweetFormData(tweet.content))
        ))
      case None    =>
        NotFound(views.html.error.page404())
    }
  }

  /*対象のツイートを更新する*/
  def update(id: Long) = Action { implicit request: Request[AnyContent] =>
    form.bindFromRequest().fold(
      (formWithErrors: Form[TweetFormData]) => {
        BadRequest(views.html.tweet.edit(id, formWithErrors))
      },
      (data: TweetFormData) => {
        tweets.find(_.id.exists(_ == id)) match{
          case Some(tweet) =>
            tweets.update(id.toInt -1, tweet.copy(content = data.content))
            Redirect(routes.TweetController.list())
          case None =>
            NotFound(views.html.error.page404())
        }
      }
    )
  }

  def delete() = Action { implicit request: Request[AnyContent] =>
    val idOpt =request.body.asFormUrlEncoded.get("id").headOption
    tweets.find(_.id.map(_.toString) == idOpt) match {
      case Some(tweet) =>
        tweets -= tweet
        //削除 -> リダイレクト
        Redirect(routes.TweetController.list())
      case None =>
        NotFound(views.html.error.page404())
    }
  }
}
