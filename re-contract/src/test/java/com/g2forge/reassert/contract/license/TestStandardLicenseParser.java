package com.g2forge.reassert.contract.license;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.license.StandardLicense;
import com.g2forge.reassert.contract.license.StandardLicenseParser;

public class TestStandardLicenseParser {
	@Test
	public void apache2Simple() {
		HAssert.assertEquals(StandardLicense.Apache2, StandardLicenseParser.create().parse("The Apache License, Version 2.0"));
	}
}
