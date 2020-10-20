package com.evolutiongaming.bootcamp.error_handling

import cats.data.ValidatedNec
// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object Homework {

  case class PaymentCard(
                          name: String,
                          number: String,
                          expirationDate: String,
                          securityCode: String
                        )

  sealed trait ValidationError
  object ValidationError {
    final case object NameError extends ValidationError
    final case object CardNumberError extends ValidationError
    final case object SecurityCodeError extends ValidationError
    final case object DateFormatError extends ValidationError
  }

  object PaymentCardValidator {
    import ValidationError._
    import cats.implicits._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateName(name: String): AllErrorsOr[String] = {
      if (name.matches("[A-Z ]+")) name.validNec
      else NameError.invalidNec
    }

    private def validateCardNumber(number: String): AllErrorsOr[String] = {
      if (number.matches("[0-9 ]+")) number.validNec
      else CardNumberError.invalidNec
    }


    private def validateExpirationDate(date: String): AllErrorsOr[String] = {
      if (date.matches("^(0[1-9]|1[0-2])\\/?([0-9]{4}|[0-9]{2})$")) date.validNec
      else DateFormatError.invalidNec
    }

    private def validateSecurityCode(csc: String): AllErrorsOr[String] = {
      if (csc.matches("[0-9]{3}")) csc.validNec
      else SecurityCodeError.invalidNec
    }

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] = {
      (validateName(name),
      validateCardNumber(number),
      validateExpirationDate(expirationDate),
      validateSecurityCode(securityCode)
      ).mapN(PaymentCard)
    }
  }
}