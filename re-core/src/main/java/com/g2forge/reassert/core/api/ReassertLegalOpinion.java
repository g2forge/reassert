package com.g2forge.reassert.core.api;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks portions of the reassert code base which encode legal opinions. For example any code which decomposes licenses into terms, or algorithms for matching
 * licenses and usages. Code which can be proven or tested correct absent a legal opinion would not be marked.
 */
@Documented
@Retention(RetentionPolicy.CLASS)
@Target({ ElementType.METHOD, ElementType.TYPE })
public @interface ReassertLegalOpinion {}
