package com.wavesplatform.it;

import org.scalatest.TagAnnotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@TagAnnotation
@Retention(RetentionPolicy.RUNTIME)
// TYPE only: IntegrationTestsPlugin.loadTestNames selects suites by class-level annotation, so a method-level tag
// would be excluded from integrationTests and never selected by loadTests.
@Target(ElementType.TYPE)
public @interface LoadTest {
}
