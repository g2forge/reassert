package com.g2forge.reassert.core.model.contract.license;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.helpers.HPrimitive;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class LicenseVersion {
	public enum Field {
		MAJOR,
		MINOR,
		PATCH;
	}

	protected final int major;

	protected final Integer minor;

	protected final Integer patch;

	public LicenseVersion(int major) {
		this(major, null, null);
	}

	public LicenseVersion(int major, int minor) {
		this(major, minor, null);
	}

	public LicenseVersion(String version) {
		final Pattern pattern = Pattern.compile("([0-9]+)(\\.([0-9]+)(\\.([0-9]+))?)?");
		final Matcher matcher = pattern.matcher(version);
		if (!matcher.matches()) throw new IllegalArgumentException(String.format("Cannot parse version \"%1$s\"!", version));
		this.major = Integer.parseInt(matcher.group(1));
		this.minor = HPrimitive.parseInteger(matcher.group(3));
		this.patch = HPrimitive.parseInteger(matcher.group(5));
	}

	public Integer get(Field field) {
		switch (field) {
			case MAJOR:
				return getMajor();
			case MINOR:
				return getMinor();
			case PATCH:
				return getPatch();
			default:
				throw new EnumException(Field.class, field);
		}
	}

	@Override
	public String toString() {
		final StringBuilder retVal = new StringBuilder();
		retVal.append(getMajor());

		final Integer minor = getMinor();
		final Integer patch = getPatch();
		if (minor != null) {
			retVal.append('.').append(minor);
			if (patch != null) retVal.append('.').append(patch);
		}
		if ((minor == null) && (patch != null)) throw new IllegalStateException();
		return retVal.toString();
	}
}
