<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xsl:output method="text" indent="no" />

    <xsl:strip-space elements="xs:sequence"/>

    <!-- Generate a name from an element -->
    <xsl:template name="gen-name">
        <xsl:param name="element"></xsl:param>
        <xsl:value-of select="/xs:schema/@id" />_<xsl:value-of select="$element/@name" />
    </xsl:template>

    <!-- Convert a type name to a name without namespace -->
    <xsl:template name="GetRefName">
        <xsl:param name="ref" />
        <xsl:choose>
            <xsl:when test="contains($ref, ':')">
                <!-- Ref has namespace prefix -->
                <xsl:value-of select="substring-after($ref, ':')" />
            </xsl:when>
            <xsl:otherwise>
                <!-- Ref has no namespace prefix -->
                <xsl:value-of select="$ref" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!-- Generate the C type from a xs:restriction -->
    <xsl:template name="gen-type">
        <xsl:param name="restriction" />
        <xsl:choose>
            <xsl:when test="$restriction/@base='xs:int'">
                <xsl:choose>
                    <xsl:when test="$restriction/xs:minInclusive/@value = 0">
                        <xsl:choose>
                            <xsl:when test="$restriction/xs:maxInclusive/@value &lt; 256">
                                <xsl:text>uint8</xsl:text>
                            </xsl:when>
                            <xsl:when test="$restriction/xs:maxInclusive/@value &lt; 65536">
                                <xsl:text>uint16</xsl:text>
                            </xsl:when>
                            <xsl:when test="$restriction/xs:maxInclusive/@value &lt; 4294967296">
                                <xsl:text>uint32</xsl:text>
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:text>uint64</xsl:text>
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:choose>
                            <xsl:when test="$restriction/xs:maxInclusive/@value &lt; 127">
                                <xsl:text>int8</xsl:text>
                            </xsl:when>
                            <xsl:when test="$restriction/xs:maxInclusive/@value &lt; 32768">
                                <xsl:text>int16</xsl:text>
                            </xsl:when>
                            <xsl:when test="$restriction/xs:maxInclusive/@value &lt; 2147483648">
                                <xsl:text>int32</xsl:text>
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:text>int64</xsl:text>
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
                <xsl:message terminate="yes">
                    Unknown type: <xsl:value-of select="$restriction" />
                </xsl:message>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!-- Start of processing -->
    <xsl:template match="/xs:schema">
        <xsl:text>
<![CDATA[
/* Generated C code */
#include <barrelfish/barrelfish.h>
]]></xsl:text>
        <xsl:apply-templates select="xs:annotation"></xsl:apply-templates>
        <xsl:for-each select="xs:simpleType">
            <xsl:call-template name="typedef-type"></xsl:call-template>
        </xsl:for-each>
        <xsl:for-each select="xs:complexType">
            <xsl:call-template name="complex-struct"></xsl:call-template>
        </xsl:for-each>
        <xsl:text>&#10;</xsl:text>
        <xsl:for-each select="xs:complexType">
            <xsl:call-template name="gen-function-header">
                <xsl:with-param name="type" select="current()" />
            </xsl:call-template>
        </xsl:for-each>
    </xsl:template>

    <xsl:template match="xs:simpleType" name="typedef-type">
        <xsl:apply-templates select="xs:annotation"></xsl:apply-templates>
        <xsl:variable name="type">
            <xsl:call-template name="gen-type">
                <xsl:with-param name="restriction" select="xs:restriction"/>
            </xsl:call-template>
        </xsl:variable>
        <xsl:text>&#10;typedef </xsl:text>
        <xsl:value-of select="$type" />
        <xsl:text> </xsl:text>
        <xsl:value-of select="@name" />
        <xsl:text>;</xsl:text>
    </xsl:template>

    <xsl:template match="xs:complexType" name="complex-struct">
        <xsl:apply-templates select="xs:annotation"></xsl:apply-templates>
        <xsl:variable name="name">
            <xsl:call-template name="gen-name">
                <xsl:with-param name="element" select="." />
            </xsl:call-template>
        </xsl:variable>
        <xsl:text>&#10;&#10;struct </xsl:text>
        <xsl:value-of select="$name" /><xsl:text> {</xsl:text>
        <xsl:apply-templates select="xs:sequence|xs:attribute"></xsl:apply-templates>
        <xsl:text>&#10;};</xsl:text>
    </xsl:template>

    <xsl:template match="xs:sequence">
        <xsl:apply-templates select="xs:element"/>
    </xsl:template>

    <xsl:template match="xs:element">
        <xsl:apply-templates select="xs:annotation"></xsl:apply-templates>
        <xsl:variable name="typeName">
            <xsl:call-template name="GetRefName">
                <xsl:with-param name="ref" select="@type"/>
            </xsl:call-template>
        </xsl:variable>

        <xsl:text>&#10;    struct </xsl:text>
        <xsl:call-template name="gen-name">
            <xsl:with-param name="element" select="/xs:schema/xs:complexType[@name = $typeName]">
            </xsl:with-param>
        </xsl:call-template>
        <xsl:text> </xsl:text>
        <xsl:value-of select="@name" />
        <xsl:text>;</xsl:text>
    </xsl:template>

    <xsl:template match="xs:attribute">
        <xsl:variable name="typeName">
            <xsl:call-template name="GetRefName">
                <xsl:with-param name="ref" select="@type"/>
            </xsl:call-template>
        </xsl:variable>
        <xsl:apply-templates select="xs:annotation"></xsl:apply-templates>
        <xsl:text>&#10;    </xsl:text>
        <xsl:value-of select="$typeName"></xsl:value-of>
        <xsl:text> </xsl:text>
        <xsl:value-of select="@name"></xsl:value-of>
        <xsl:text>;</xsl:text>
    </xsl:template>

    <xsl:template name="gen-function-header">
        <xsl:param name="type" />
        <xsl:text>&#10;errval_t </xsl:text>

        <xsl:variable name="name">
            <xsl:call-template name="gen-name">
                <xsl:with-param name="element" select="$type" />
            </xsl:call-template>
        </xsl:variable>

        <xsl:value-of select="$name" />
        <xsl:text>_add(struct </xsl:text>
        <xsl:value-of select="$name" />
        <xsl:text>*);</xsl:text>
    </xsl:template>

    <xsl:template match="xs:annotation">
/**
<xsl:for-each select="tokenize(current()/xs:documentation, '\r?\n')">
 * <xsl:sequence select="."/>
</xsl:for-each> 
 */
    </xsl:template>
</xsl:stylesheet>