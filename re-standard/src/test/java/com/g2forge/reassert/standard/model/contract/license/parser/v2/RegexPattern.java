package com.g2forge.reassert.standard.model.contract.license.parser.v2;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.g2forge.alexandria.analysis.ISerializableFunction1;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.java.function.IFunction1;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter(AccessLevel.PROTECTED)
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class RegexPattern<Result> implements IPattern<Result> {
	@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
	@Getter(AccessLevel.PROTECTED)
	protected static abstract class ARegexPatternBuilder<Result, Built, Builder> implements IPartialPatternBuilder<Void, Result, RegexPattern<?>, Built, Builder> {
		protected final RegexBuilder builder;

		protected boolean active = true;

		@Override
		public Builder alt(RegexPattern<?> pattern0, RegexPattern<?> pattern1, RegexPattern<?>... patterns) {
			assertActive();
			builder.startGroup(null);
			withInternal(pattern0);
			getBuilder().append('|');
			withInternal(pattern1);
			for (RegexPattern<?> pattern : patterns) {
				getBuilder().append('|');
				withInternal(pattern);
			}
			builder.endGroup();
			return getThis();
		}

		protected void assertActive() {
			if (!active) throw new IllegalStateException("Attempt to manipulate inactive builder, perhaps you forgot to close the parent?");
		}

		@Override
		public ICharacterClassBuilder<? extends Builder> charClass() {
			assertActive();

			active = false;
			getBuilder().append('[');
			final Builder retVal = getThis();
			return new ICharacterClassBuilder<Builder>() {
				protected boolean active = true;

				protected void assertActive() {
					if (!active) throw new IllegalStateException("Attempt to manipulate inactive builder, perhaps you forgot to close the parent?");
				}

				@Override
				public Builder build() {
					assertActive();
					getBuilder().append(']');
					((ARegexPatternBuilder<?, ?, ?>) retVal).active = true;
					active = false;
					return retVal;
				}

				@Override
				public ICharacterClassBuilder<Builder> character(char character) {
					assertActive();
					getBuilder().append(character);
					return this;
				}

				@Override
				public ICharacterClassBuilder<Builder> named(NamedCharacterClass named) {
					assertActive();
					getBuilder().append(named.getRegex());
					return this;
				}

				@Override
				public ICharacterClassBuilder<Builder> range(char start, char end) {
					assertActive();
					getBuilder().append(start).append('-').append(end);
					return this;
				}
			};
		}

		protected abstract Builder getThis();

		@Override
		public IGroupBuilder<Void, Result, RegexPattern<?>, ? extends Builder> group(ISerializableFunction1<? super Result, ?> field, Void arguments) {
			assertActive();
			if (arguments != null) throw new IllegalArgumentException("Arguments must be null!");

			final RegexBuilder builder = getBuilder();
			getBuilder().startGroup(field == null ? null : field.asMethodAnalyzer().getPath());
			active = false;
			return new RegexGroupBuilder<>(builder, getThis());
		}

		@Override
		public Builder named(NamedCharacterClass named) {
			return pattern(named.getRegex());
		}

		@Override
		public Builder opt() {
			return pattern("?");
		}

		protected Builder pattern(final String string) {
			assertActive();
			getBuilder().append(string);
			return getThis();
		}

		@Override
		public Builder plus() {
			return pattern("+");
		}
		
		@Override
		public Builder star() {
			return pattern("*");
		}

		@Override
		public Builder text(String text) {
			return pattern(Pattern.quote(text));
		}

		@Override
		public Builder with(RegexPattern<?> pattern) {
			assertActive();
			withInternal(pattern);
			return getThis();
		}

		protected void withInternal(RegexPattern<?> pattern) {
			final RegexBuilder builder = getBuilder();
			builder.append(pattern.getPattern().toString());
			for (Map.Entry<String, Integer> entry : pattern.getFields().entrySet()) {
				builder.fields.put(entry.getKey(), entry.getValue() + builder.nGroups);
			}
			builder.nGroups += pattern.getNGroups();
		}
	}

	protected static class RegexBuilder {
		protected final StringBuilder builder = new StringBuilder();

		protected int nGroups = 0;

		protected final Map<String, Integer> fields = new HashMap<>();

		public RegexBuilder append(char text) {
			builder.append(text);
			return this;
		}

		public RegexBuilder append(String text) {
			builder.append(text);
			return this;
		}

		public RegexBuilder endGroup() {
			return append(')');
		}

		public RegexBuilder startGroup(String field) {
			++nGroups;
			if (field != null) fields.put(field, nGroups);
			return append('(');
		}
	}

	@Getter(AccessLevel.PROTECTED)
	protected static class RegexGroupBuilder<Result, ParentBuilder> extends ARegexPatternBuilder<Result, ParentBuilder, IGroupBuilder<Void, Result, RegexPattern<?>, ParentBuilder>> implements IGroupBuilder<Void, Result, RegexPattern<?>, ParentBuilder> {
		protected final ParentBuilder parent;

		protected RegexGroupBuilder(RegexBuilder builder, ParentBuilder parent) {
			super(builder);
			this.parent = parent;
		}

		@Override
		public ParentBuilder build() {
			((ARegexPatternBuilder<?, ?, ?>) parent).active = true;
			this.active = false;
			final RegexBuilder builder = getBuilder();
			builder.endGroup();
			return parent;
		}

		@Override
		protected RegexGroupBuilder<Result, ParentBuilder> getThis() {
			return this;
		}
	}

	@Getter(AccessLevel.PROTECTED)
	protected static class RegexPatternBuilder<Result> extends ARegexPatternBuilder<Result, RegexPattern<Result>, IPatternBuilder<Void, Result, RegexPattern<?>, RegexPattern<Result>>> implements IPatternBuilder<Void, Result, RegexPattern<?>, RegexPattern<Result>> {
		protected RegexPatternBuilder() {
			super(new RegexBuilder());
		}

		@Override
		public RegexPattern<Result> build(IFunction1<? super IFunction1<ISerializableFunction1<? super Result, ?>, String>, Result> input) {
			final RegexBuilder builder = getBuilder();
			final Pattern pattern = Pattern.compile(builder.builder.toString());
			return new RegexPattern<>(pattern, builder.nGroups, Collections.unmodifiableMap(builder.fields), input);
		}

		@Override
		protected RegexPatternBuilder<Result> getThis() {
			return this;
		}
	}

	public static <T> IPatternBuilder<Void, T, RegexPattern<?>, RegexPattern<T>> builder() {
		return new RegexPatternBuilder<T>();
	}

	protected final Pattern pattern;

	protected final int nGroups;

	protected final Map<String, Integer> fields;

	protected final IFunction1<? super IFunction1<ISerializableFunction1<? super Result, ?>, String>, Result> constructor;

	@Override
	public IOptional<Result> match(String string) {
		final Matcher matcher = getPattern().matcher(string);
		if (!matcher.matches()) return NullableOptional.empty();

		if (getConstructor() != null) return NullableOptional.of(getConstructor().apply(new IFunction1<ISerializableFunction1<? super Result, ?>, String>() {
			@Override
			public String apply(ISerializableFunction1<? super Result, ?> field) {
				final String path = field.asMethodAnalyzer().getPath();
				final Integer group = fields.get(path);
				if (group == null) throw new IllegalArgumentException(String.format("Field %1$s was not recognized as a group!", field.asMethodAnalyzer().getPath()));
				return matcher.group(group);
			}
		}));
		else return NullableOptional.of(null);
	}

	@Override
	public String toString() {
		return getPattern().toString();
	}
}
